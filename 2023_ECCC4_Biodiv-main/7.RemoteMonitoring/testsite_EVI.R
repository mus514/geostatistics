# Test the methodology on a subset of the sites

# Load useful paths, packages and functions
source("5.RemoteMonitoring/utils.R")

# Load the target sites
site <- file.path(path_data, "PlantationSites/target_sites_coulees.shp") |>
  sf::st_read(quiet = TRUE) |>
  dplyr::filter(SiteCode == "CDQ_EB_Chrono2", ref_id == "CDQ_EB_Foret1")

cql_roibox <- site |>
  sf::st_transform(4326) |>
  sf::st_bbox() |>
  rstac::cql2_bbox_as_geojson()

# test1: summer of 1984
# Define the STAC API endpoint for the Planetary Computer
stac_endpoint <- "https://planetarycomputer.microsoft.com/api/stac/v1"

# Create a STAC client
stac_client <- rstac::stac(stac_endpoint)

# date range 
yrange <- seq(1984, 2024, by = 2)

# storage
evis <- data.frame(
  year = double(),
  program = double(),
  evi = double()
)

for (y in yrange) {
  # Define the dates
  y <- y
  t1 <- paste0(y, "-07-01")
  t2 <- paste0(y, "-09-15")
  cql_timerange <- rstac::cql2_interval(t1, t2)
  
  # Define the search parameters
  cql_query <- stac_client |>
    rstac::ext_filter(
        collection == "landsat-c2-l2" &&
        t_intersects(datetime, {{cql_timerange}}) &&
        s_intersects(geometry, {{cql_roibox}}) && 
        `eo:cloud_cover` < 10
    ) |>
    rstac::post_request()
  # Perform the search
  items <- rstac::items_sign(
    cql_query,
    rstac::sign_planetary_computer()
  )
  lyrs <- lapply(
    items$features, 
    \(x) data.frame(id = x$id, platform = x$properties$platform)
  ) |> 
    do.call(what = rbind)
  print(y)
  print(lyrs)
  
  if (!is.null(lyrs)) {
    for (j in seq_len(nrow(lyrs))) {
        prg <- strsplit(lyrs[j, "platform"], "-")[[1]][2] |>
          as.numeric()
        evi_l <- veg_indices(items, j, prg) |>
          terra::global(mean, na.rm = TRUE)
        newline <- c(y, prg, evi_l$mean)
        evis <- rbind(evis, newline)
    }
  }
}







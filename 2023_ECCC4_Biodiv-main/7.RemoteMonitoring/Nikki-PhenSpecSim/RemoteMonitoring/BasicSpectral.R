source("5.RemoteMonitoring/utils.R")

# area of interest and targetted open areas for reforestation
roi <- sf::st_read("dat/friches/roi.gpkg")
friches <- sf::st_read("dat/friches/friches.gpkg")

# transform the roi into a bbox for the stac query
roi_box <- roi |>
  sf::st_transform(4326) |>
  sf::st_bbox()
# transform querie parameters to match CQL
cql_roibox <- rstac::cql2_bbox_as_geojson(roi_box)

# years 
years <- c(2017:2022)

for (y in years) {
  cat(
    crayon::blue(
      paste0("\u2139 - Starting year ", y, "\n"))
  )
  t1 <- paste0(y, "-07-01")
  t2 <- paste0(y, "-09-15")
  cql_timerange <- rstac::cql2_interval(t1, t2)

  # cql query to planetray computer
  cql_query <- rstac::stac("https://planetarycomputer.microsoft.com/api/stac/v1") |>
    rstac::ext_filter(
      collection == "landsat-c2-l2" &&
        t_intersects(datetime, {{cql_timerange}}) &&
        s_intersects(geometry, {{cql_roibox}}) && 
        platform == "landsat-8" &&
        `eo:cloud_cover` < 10
    ) |>
    rstac::post_request()

  # sign-in (no account required)
  signed_stac_query <- rstac::items_sign(
    cql_query,
    rstac::sign_planetary_computer()
  )
  bands <- c("nir08", "swir22", "red", "blue", "green", "swir16")
  rstac::assets_download(signed_stac_query, bands)
  dir_to <- paste0("dat/landsat/", y, "/")
  if (!dir.exists(dir_to)) dir.create(dir_to)
  lc08files <- list.files("landsat-c2", recursive = TRUE, full.names = TRUE)
  lc08files_names <- lapply(lc08files, function(x) strsplit(x, "/")[[1]][9]) |> unlist()
  file.copy(lc08files, paste(dir_to, lc08files_names, sep = ""))
  unlink("landsat-c2", recursive = TRUE)
}

# explore the metadata
lyrs <- lapply(
  signed_stac_query$features, 
  \(x) data.frame(id = x$id, platform = x$properties$platform)
) |> 
  do.call(what = rbind)

# get the urls
red_urls <- rstac::assets_url(signed_stac_query, "red")
nir_urls <- rstac::assets_url(signed_stac_query, "nir08")
blue_urls <- rstac::assets_url(signed_stac_query, "blue")

# EVI 
evi <- function(mpc_query, i) {
    redband <- rstac::assets_url(mpc_query, "red")[i] |>
      terra::rast()
    blueband <- rstac::assets_url(mpc_query, "blue")[i] |>
      terra::rast()
    nirband <- rstac::assets_url(mpc_query, "nir08")[i] |>
      terra::rast()
    evi_val <- 2.5 * ((nirband - redband) / (nirband + 6 * redband - 7.5 * blueband + 1))
    return(evi_val)
}

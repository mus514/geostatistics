# this scripts uses the hydrological data of Quebc to calculate distance to water

source("RemoteMonitoring/utils.R")

# aoi
aoi <- "P:/Database/AdminRegion/QC/reg_admin/regio_s.shp" |>
  sf::st_read(quiet = TRUE) |>
  dplyr::filter(RES_NM_REG == "Centre-du-Québec") |>
  sf::st_transform(crs = 32198)
sf::st_write(aoi, "dat/aoi.gpkg")

# hydrological network
aoi_blocks <- "P:/Database/Hydrology/QC/hyd_network/GRHQ/Index_GRHQ.shp" |> #the hydrological network data is tiled
  sf::st_read(quiet = TRUE) |> #open the tile index
  sf::st_transform(sf::st_crs(aoi)) |> #transform into the same crs as aoi
  sf::st_make_valid() |> #make valid (doesn't work otherwise)
  sf::st_intersection(aoi) #get the tiles ID

# major blocs are divided into smaller ones in our database,
# we need to check them for overlap
# db path:
hdir <- "P:/Database/Hydrology/QC/hyd_network/GRHQ/"
# object to store outputs
tileopen <- c()
# check overlap for smaller blocks
for (i in seq_len(nrow(aoi_blocks))) {
  print(i)
  # id major tile (bloc)
  tile <- aoi_blocks$Bloc[i]
  # path to all corresponding smaller blocks
  tmppath <- paste0(hdir, "bloc_", tile, "/")
  # list them
  tmpfiles <- list.files(tmppath, full.names = TRUE)
  for (j in seq_along(tmpfiles)) { #for all smaller blocks
    # open and transform to correct scr
    tmp <- sf::st_read(tmpfiles[j], quiet = TRUE, layer = "UDH") |>
      sf::st_transform(crs = sf::st_crs(aoi))
    # check intersection
    is_inter <- sf::st_intersects(tmp, aoi)
    # if intersects, add to list of files to open
    if (length(is_inter[[1]]) != 0) {
      tileopen <- c(tileopen, tmpfiles[j])
    } #if
  } #smaller blocks (j)
} #larger blocs (i)
streams <- lapply(
  tileopen,
  function(x) {
    sf::st_read(x, layer = "RH_L", quiet = TRUE) |>
      dplyr::filter(TYPECE == 10)
  }
  ) %>% 
  do.call("rbind", .) |>
  # drop m_range
  sf::st_zm(drop = TRUE) |>
  # transform
  sf::st_transform(sf::st_crs(aoi)) |>
  # intersect
  sf::st_intersection(aoi)

surf <- lapply(
  tileopen,
  function(x) {
    sf::st_read(x, layer = "RH_S", quiet = TRUE) |>
      dplyr::filter(TYPECE == 10)
  }
  ) %>% 
  do.call("rbind", .) |>
  # transform
  sf::st_transform(sf::st_crs(aoi)) |>
  # intersect
  sf::st_intersection(aoi)
sf::st_write(surf, "dat/surface_water.gpkg", append = FALSE)

#add buffer to transform into polygon
# (can't rasterize otherwise)
streams_pol <- streams |> 
  sf::st_buffer(2) %>%
  # cast to one type
  sf::st_cast("MULTIPOLYGON")
streams_pol <- rbind(streams_pol, surf)
# export
sf::st_write(streams_pol, "dat/streams.gpkg")

# rasterize
# template
r <- raster::raster(
  ext = raster::extent(aoi),
  crs = sf::st_crs(aoi),
  resolution = 5
)
# rasterization (5m res, same extent as aoi)
hyd_r <- fasterize::fasterize(streams_pol, r) |> #buffered streams
  terra::rast()
hyds_r <- fasterize::fasterize(surf, r) |> #larger surfaces of water
  terra::rast()
crs(hyd_r) <- crs(r)
crs(hyds_r) <- crs(r)
hyd_r[hyds_r == 1] <- 1
writeRaster(
    hyd_r,
    "dat/hydro_net.tif",
    overwrite = TRUE
)

## THIS TAKES TOO LONG, USE rgrass ##
# distance to streams
#dist_water <- terra::distance(hyd_r)

# Initialize GRASS
rgrass::initGRASS(
  gisBase = "C:/Program Files/GRASS GIS 8.3",
  home = tempdir(),
  override = TRUE,
  SG = terra::rast(r)
)
#Write the raster file to GRASS
rgrass::write_RAST(
  as(raster(hyd_r), "SpatialGridDataFrame"), #needs to be a spatialgriddf
  "rasterFile",
  overwrite = TRUE
)
rgrass::execGRASS(
  "g.region", flags = "p", parameters = list(raster = "rasterFile")
)
# Calculate distance to this raster
rgrass::execGRASS(
  "r.grow.distance",
  flags = "overwrite",
  parameters = list(input = "rasterFile", distance = "d2")
)
dist_water <- rgrass::read_RAST("d2") |>
  raster::raster() |>
  terra::mask(aoi)
# for some reason it doesn't work if we try to transform into rast directly
crs(dist_water) <- crs(r)
writeRaster(
  dist_water,
  "dat/dist_water.tif",
  overwrite = TRUE
)

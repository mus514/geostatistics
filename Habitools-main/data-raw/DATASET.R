# example_montreal_sf
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create area over montreal
polygon_coords <- matrix(
  c(-73.5700, 45.5000,  # Bottom-left
    -73.5650, 45.5000,  # Bottom-right
    -73.5650, 45.5050,  # Top-right
    -73.5700, 45.5050,  # Top-left
    -73.5700, 45.5000   # Close polygon
  ),
  ncol = 2, byrow = TRUE
)
example_montreal_sf <- sf::st_sfc(sf::st_polygon(list(polygon_coords)), crs = 4326) |>
  sf::st_as_sf()
example_montreal_sf <- dplyr::rename(example_montreal_sf,geometry=x)

# save package data in the correct format
usethis::use_data(example_montreal_sf, overwrite = TRUE)

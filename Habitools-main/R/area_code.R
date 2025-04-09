## ## ## ## ## ## ## ## ## ## ## #
## LULC exact area calculation
## ## ## ## ## ## ## ## ## ## ## #
# Author : Marc-Olivier Beausoleil
# Date : 2024-12-17


#' Extract exact area
#'
#' @param rast Raster input with categories to calculate the area
#' @param unit Units passed to cellSize and calculate the area
#'
#' @returns Data frame with the code name (first column) and the associated area (second column)
#' @source https://gis.stackexchange.com/questions/433375/calculate-area-for-raster-in-r
#' @export
#'
#' @examples
#' \dontrun{
#' f <- system.file("ex/elev.tif", package="terra")
#' r <- rast(f)
#' area_code(r)
#' }
area_code <- function(rast, unit = "m") {
  # Get cellsize (some cells might vary in size to take that into account)
  a <- terra::cellSize(rast, unit=unit)

  # minmax(a) # show the range of cellsize

  # Zonal statistics based on LULC categories
  z <- terra::zonal(a, rast, sum, na.rm=TRUE)
  return(z)
}

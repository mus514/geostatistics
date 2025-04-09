#' Get a detailed description report from a raster file
#'
#' @param pathRaster Path to the raster from which we want to get description
#'
#' @return List of 'terra::describe()' output, 'NoData Value' and data type from 'terra::datatype()'
#' @export
#' @import terra
#' @examples
#' \dontrun{
#' f <- system.file("ex/elev.tif", package="terra")
#' descRast(f)
#' }
descRast <- function(pathRaster) {
  # Load the raster
  load.rast = terra::rast(pathRaster)
  # Get description of the file
  desc.file = terra::describe(x = pathRaster)
  # Get the datatype
  data.type = terra::datatype(x = load.rast)

  # Search for  a field
  v <- grep(pattern = "NoData Value",
            x = desc.file,
            value = TRUE)

  # Extract number of the NoData field
  nodata = strsplit(v, "=")[[1]][2] |>
    as.numeric()

  return(list(description = desc.file,
              noData = nodata,
              dataType = data.type))
}

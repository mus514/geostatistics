################################################################################
## Utility functions for finalizing project and final datapackage
################################################################################
# date: 2025-01-14
# authors: Marc-Olivier Beausoleil based on scripts developped by Mederic
# Contains functions to :
# --> scale rasters
# --> Process the raster to prepare them for final datapackage
# --> Change pixel values from a raster to the desired units taking into
#     account the area of each pixel

#' Rescale the raster values between 0 and 1.
#'
#' @param r A raster
#'
#' @description
#' Uses the data in the raster (min and max) to transform the scale of the
#' raster to be between 0 and 1 following the equation (r - min) / (max - min).
#'
#' @returns raster values between 0 and 1
#' @source https://gis.stackexchange.com/questions/437520/normalize-raster-in-r
#' @export
#'
#' @examples
#' f <- system.file("ex/elev.tif", package="terra")
#' r <- terra::rast(f)
#' scale_terra(r)
scale_terra <- function(r) {
  mm <- terra::minmax(r)
  (r - mm[1,]) / (mm[2,] - mm[1,])
}


#' Change the format (CRS, extent, resolution, normalization) of an input raster to the desired properties
#'
#' @param input Object with SpatRaster class or a Path to a raster (class character)
#' @param roi_r Raster of the ROI with CRS and used to resample
#' @param roi Polygon of the ROI
#' @param project Should reproject to the roi_r ?
#' @param resample Should resample to the roi_r ?
#' @param normalize Should normalize raster values ?
#' @param scale Should scale raster ?
#'
#' @description
#' Generic function that takes input raster and prepares it for final data
#' package. Can reproject, resample, normalize (using the `scale_terra()`),
#' standardize (z-score with `terra::scale()` function).
#'
#' @returns a Raster with the selected transformations
#' @export
#'
#' @examples
#' f <- system.file("ex/elev.tif", package="terra")
#' r <- terra::rast(f)
# process_raster(r, r, terra::as.polygons(r), normalize = TRUE)
process_raster <- function(input,
                           roi_r,
                           roi,
                           project = TRUE, resample = TRUE,
                           normalize = FALSE, scale = FALSE) {

  if(missing(roi_r)) stop('roi_r a required raster template object')
  if(missing(roi)) stop('roi a required spatial vector object')

  # If input is a Spatraster, will process the file directly
  if (inherits(input,"SpatRaster")) {
    raster = input
  }
  # If input is path (character) with READ the raster
  if (inherits(input,"character")) {
    raster <- terra::rast(input)
  }
  # Reproject raster with roi_raster
  if (project == TRUE) {raster <- terra::project(raster, terra::crs(roi_r))}
  # Resample raster with roi_raster
  if (resample == TRUE) {raster <- terra::resample(raster, y = roi_r)}
  # Crop and mask
  raster <- terra::crop(raster, y = roi, mask = TRUE)
  # Normalize
  if (normalize == TRUE) {raster <- scale_terra(raster)}
  # scale
  if (scale == TRUE) {raster <- terra::scale(raster)}
  return(raster)
}



#' Change units of raster to values/ha
#'
#' @param r Raster
#' @param ha2px FALSE (default) will compute *pixel value* to *pixel value / hectar*. TRUE, will compute *pixel/ha* to *pixel value*
#' @param unit Units (default = 'ha') passed to `terra::cellSize()`
#' @param ... more arguments passed to `terra::cellSize()`
#'
#' @description
#' Change the units of a raster while taking into account the actual pixel area
#' with the `terra::cellSize()` function. If ha2px is FALSE (default), will
#' calculate `r/cellarea` or will divide the values of each pixel (assuming
#' it is unit) with the pixel area in hectar. If ha2px is TRUE (assuming
#' it is unit/ha) the `r*cellarea` will ouput each pixel in terms of `unit`.
#'
#' @returns Raster with the values divided by each pixel area
#' @export
#'
#' @examples
#' # Illustrative example, but not real example
#' f <- system.file("ex/elev.tif", package="terra")
#' r <- terra::rast(f)
#' px2ha(r)
#' @source https://stackoverflow.com/questions/79353000/what-is-the-proper-way-to-change-the-units-of-a-raster-to-account-for-the-area-o
px2ha <- function(r, ha2px = FALSE, unit = 'ha', ...) {
  cellarea = terra::cellSize(r, unit = 'ha', ...)
  # message('Mean pixel area (ha): ', round(mean(cellarea[]), 2))
  if (ha2px) {
    r*cellarea # px/ha to px values
  } else {
    r/cellarea # px2ha
  }
}

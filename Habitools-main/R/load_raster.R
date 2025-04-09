
################################################################################
## Load and crop a COG file from a blob
################################################################################
#date: 2024-11-29
#authors: KJ Theron
#
#' Internal function calculate_bbox
#' @description
#' Calculates the bounding box for an area of interest
#' @param aoi sf geometry or SpatRaster, study region
#' @param to_wgs84 Boolean, to transform bbox to EPSG 4326 or keep original CRS
#' @return numerical vector, containing bounding box coordinates
calculate_bbox <- function(aoi,to_wgs84=FALSE) {
     # If SpatRaster
     if(base::class(aoi)[1]=="SpatRaster"){
          # Transform
          if(to_wgs84==TRUE){
               from <- terra::crs(aoi,describe=TRUE)$code
               from <- base::paste0("epsg:",from)
               to <- "epsg:4326"
               bbox <- terra::ext(aoi)
               bbox <- terra::project(bbox,from,to)
          } else{
               bbox <- terra::ext(aoi)
          }
          bbox <- c(
               base::as.numeric(bbox[1]),
               base::as.numeric(bbox[2]),
               base::as.numeric(bbox[3]),
               base::as.numeric(bbox[4]))
     # If sf object
     } else{
          # Transform
          if(to_wgs84==TRUE){
               aoi <- sf::st_transform(aoi,4326)
          }

          # Calculates bbox coordinates
          bbox <- sf::st_bbox(aoi)
          bbox <- c(
               base::as.numeric(bbox[1]),
               base::as.numeric(bbox[2]),
               base::as.numeric(bbox[3]),
               base::as.numeric(bbox[4]))
     }

     # Return
     base::return(bbox)
}
#
#' Load raster data
#' @description
#' Loads a COG file into memory using a http link and AOI for selecting pixels to load
#'
#' @param COG_link Character, a http link to a COG file
#' @param aoi sf geometry, area to be used to select pixels within the raster
#' @param save_dir Character, path to save the raster
#' @param desc If TRUE, will describe the raster (terra::describe)
#' @param ... Additionnal arguments to terra::rast()
#'
#' @return SpatRast, containing pixels from the provided COG link within the AOI
#' @export
#' @examples
#' \dontrun{
#' library(terra)
#' library(sf)
#'
#' # Create AOI
#' coords <- base::matrix(c(
#'      -405086.7, 181125.8,
#'      -405086.7, 184647.8,
#'      -400843.1, 184647.8,
#'      -400843.1, 181125.8,
#'      -405086.7, 181125.8),
#'      ncol = 2, byrow = TRUE)
#' aoi <- sf::st_polygon(list(coords)) |>
#'      sf::st_sfc(crs = 32198) |>
#'      sf::st_sf(crs = 32198)
#'
#' # Grab Quebec LULC link
#' COG_link <- query_lookup(subject="utilisation-territoire",year=2020)$file_path
#'
#' # Load LULC within the AOI
#' lulc <- load_raster(COG_link=COG_link,aoi=aoi)
#'
#' # Plot
#' plot(lulc)
#' }
load_raster <- function(COG_link=NULL,aoi=NULL,save_dir=NULL, desc = FALSE, ...) {

  # Create/assign SAS
  sas <- base::Sys.getenv("SAS_TOKEN")
  if(sas==""){
    cli::cli_abort("There is no sas token. Please create one.")
  } else{
    # Is SAS valid?
    sas_validity <- is.sas.valid(sas)
    if(sas_validity[[1]] < 0) {
         cli::cli_abort("The sas token is NOT valid. Please create a new one.")
    }         
  }

  # Prepare Cog_link with sas
  COG_link <- base::sub("\\?.*$", "", COG_link)
  COG_link <- paste0(COG_link,"?",sas)

  # If describe True, will report the description of the raster
  if (desc) {
    cli::cli_alert_info("Describing raster...")
    desc_raster = terra::describe(x = COG_link)
    base::print(desc_raster)
  }

  # Grab tif header
  tif<-terra::rast(COG_link,vsi=TRUE, ...)

  # Extract the CRS code
  crs_code = terra::crs(tif,describe=TRUE)$code

  # If CRS of tif is NA (unamed or missing) still use the CRS but make the aoi a SpatVector
  # Transform the AOI and make sure the wgs84 is FALSE to get the same extent as RASTER
  if (is.na(crs_code) & !base::is.null(aoi)) {
    cli::cli_alert_info("Projecting aoi...")

    # Still get the CRS from the raster
    prj <- terra::crs(tif)

    # Reproject based on the CRS of the raster
    pol <- terra::project(terra::vect(aoi), prj)

    # Calculate the bbox
    bbox <- calculate_bbox(aoi=sf::st_as_sf(pol), to_wgs84 = FALSE)

    # Change the AOI to be a spatVector (so the class can be set properly)
    aoi = pol
  }

  # Transform AOI
  if(!base::is.null(aoi) & base::any(base::class(aoi)=='sf')){
    cli::cli_alert_info("Projecting aoi...")

    aoi <- sf::st_transform(aoi,base::as.numeric(crs_code))

    # Calculate the bbox
    bbox <- calculate_bbox(aoi=aoi)
  }

  # Load raster
  if(!base::is.null(aoi)){
    cli::cli_alert_info("Loading and cropping raster...")

    # Load and crop layer
    COG <- terra::rast(COG_link, vsi = TRUE, win = bbox, ...) |>
      terra::crop(aoi) |>
      terra::mask(aoi)
  } else{
    cli::cli_alert_info("Loading raster...")
    COG <- terra::rast(COG_link,vsi=TRUE, ...)
  }

  # Save
  if(!base::is.null(save_dir)){
    cli::cli_alert_info("Saving SpatRaster to disk...")
    terra::writeRaster(COG,save_dir,overwrite=TRUE)
  }

  # Return
  cli::cli_alert_success("Raster loaded.")
  base::return(COG)
}


################################################################################
## Function to resample, project, crop, mask, and align a raster to a reference grid
################################################################################
#date: 2024-11-29
#authors: KJ Theron
#
#' Aligns a raster to reference grid
#' @description 
#' Reprojects, resamples, aligns, crops, and masks SpatRaster to template raster
#' @param raster SpatRaster, raster to process
#' @param temp_grid SpatRaster, template grid used for aligning (from createRastTemp() function)
#' @param method Character, method of interpolation. E.g. "bilinear", "near". See terra::project()
#' @param clamp Boolean or numeric vector, to clamp min and max after reprojecting. TRUE will automatically calcuate min max, or custom vector with min max values e.g. c(0,100)
#' @param save_dir Character, path to save the raster
#' @return SpatRast, with the provided specifications taken from the reference grid
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
#' raster <- load_raster(COG_link=COG_link,aoi=aoi)
#' 
#' # Create template for resampling
#' temp <- create_rast_template(aoi=aoi,crs=2961,res=50)
#' 
#' # Align raster to grid
#' aligned_raster <- align_raster(raster=raster,temp_grid=temp,method="near")
#' 
#' # Test alignment
#' terra::compareGeom(aligned_raster, temp, stopOnError=FALSE)
#' terra::compareGeom(raster, temp, stopOnError=FALSE)
#' }
align_raster <- function(raster,temp_grid,method,clamp=FALSE,save_dir=NULL) {
     # Check for colours
     has_colours<-terra::has.colors(raster)
     if(has_colours==TRUE){
          colours<-terra::coltab(raster)
     }

     # Crop first, to reduce amount of pixels to process
     if(!terra::crs(raster)==terra::crs(temp_grid)){
          # Reproject
          cli::cli_alert_info("CRS's dont match. Reprojecting template...")
          base::suppressWarnings(temp_pro<-temp_grid |>
               terra::project(terra::crs(raster)))
          
          # Crop
          cli::cli_alert_info("Cropping SpatRaster...")
          raster<-raster |>
               terra::crop(terra::ext(temp_pro))
     } else{
          # Crop
          cli::cli_alert_info("Cropping SpatRaster...")
          raster<-raster |>
               terra::crop(terra::ext(temp_grid))
     }

     # Save min max for clamping
     if(is.logical(clamp)){
          if(clamp==TRUE){
               min_max <- terra::minmax(raster,compute=TRUE)
          }
     }
     
     # Update pixel extent to ensure alignment
     cli::cli_alert_info("Projecting, aligning, and resampling...")
     if(!terra::crs(raster)==terra::crs(temp_grid)){
          temp_pro<-terra::project(temp_grid,terra::crs(raster))
          raster<-terra::resample(
               raster,
               temp_pro,
               method = method,
               threads = TRUE)
     } else{
          raster<-terra::resample(
               raster,
               temp_grid,
               method = method,
               threads = TRUE)
     }

     # Project     
     raster<-terra::project(
          raster,
          temp_grid,
          method=method,
          align=TRUE,
          threads=TRUE,
          overwrite=TRUE)     

     # Check if they are aligned
     aligned <- terra::compareGeom(raster, temp_grid, stopOnError=FALSE)
     if(aligned==FALSE){
          # Update pixel extent to ensure alignment
          raster<-terra::resample(raster, temp_grid, method = method)
     }

     # Clamp
     if(base::is.numeric(clamp)){
          cli::cli_alert_info("Clamping...")
          raster <- terra::clamp(
               x=raster,
               lower=clamp[1],
               upper=clamp[2],
               values=TRUE)
     }
     if(base::is.logical(clamp)){
          cli::cli_alert_info("Clamping...")
          if(clamp==TRUE){
               raster <- terra::clamp(
                    x=raster,
                    lower=min_max[1],
                    upper=min_max[2],
                    values=TRUE)
          }
     }
     
     # Mask
     cli::cli_alert_info("Masking...")
     raster<-terra::crop(raster,temp_grid,mask=TRUE)
     
     # Add colours
     if(has_colours==TRUE){
          terra::coltab(raster)<-colours
     }
     
     # Save to disk
     if(!base::is.null(save_dir)){
          cli::cli_alert_info("Saving SpatRaster to disk...")
          terra::writeRaster(
               raster,
               save_dir)
     }

     # Return
     cli::cli_alert_success("Raster aligned.")
     base::return(raster)
}

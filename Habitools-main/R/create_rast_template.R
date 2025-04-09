
################################################################################
## Create a reference raster layer to be used for reprojecting other layers
################################################################################
#date: 2024-11-29
#authors: KJ Theron
#
#' Load raster data
#' @description 
#' Creates a raster grid from a geometry to be used for resampling other rasters
#' @param aoi sf geometry, area to be used to select pixels within the raster
#' @param crs Numeric, coordinate reference system as EPSG code
#' @param res Numeric, pixel resolution
#' @param val Numeric, value to fill the pixels
#' @param save_dir Character, path to save the raster
#' @return SpatRast, with the provided specifications
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
#' # Create template
#' tmp_rast <- create_rast_template(aoi=aoi,crs=32198,res=50)
#' 
#' # Plot
#' plot(tmp_rast)
#' }
create_rast_template <- function(aoi,crs,res,val=1,save_dir=NULL) {
     # Transform
     aoi_temp<-sf::st_transform(aoi,crs)

     # Create raster template of shapefile
     rast_temp<-terra::rast(
          xmin=terra::xmin(terra::vect(aoi_temp)),
          xmax=terra::xmax(terra::vect(aoi_temp)),
          ymin=terra::ymin(terra::vect(aoi_temp)),
          ymax=terra::ymax(terra::vect(aoi_temp)),
          crs=base::paste0("epsg:",base::as.character(crs)),
          res=res,
          vals=val) |>
          terra::crop(aoi_temp,mask=TRUE)
     
     # Assign NA
     terra::NAflag(rast_temp)=-127

     # Save to disk
     if(!base::is.null(save_dir)){
          terra::writeRaster(
               x=rast_temp,
               filename=save_dir,
               datatype="INT1S",
               NAflag=-127,
               overwrite=TRUE)
     }
          
     # Return
     base::return(rast_temp)
}

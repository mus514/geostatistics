
################################################################################
## Loading geo-parquet files from Habitat blob 
################################################################################
#date: 2025-03-24
#authors: KJ Theron
# https://github.com/cboettig/duckdbfs/tree/main
# https://github.com/duckdb/duckdb-r/issues/55
#
#' Internal function prep_blob_paths
#' @description 
#' Prepares the file paths form the blob for DuckDB with SAS tokens added
#' @param http_link Character, link to the parquet file to load
#' @param link_expiration_time Double, duration of sas token for downloading (minutes)
#' @return Character, path of file
prep_blob_paths <- function(http_link,link_expiration_time){
     # Remove SAS if present
     http_link <- base::sub("\\?.*$", "", http_link)

     # Construct the folder path from the split components
     split_http <- base::unlist(base::strsplit(http_link, "/"))    
     folder_path <- base::paste0(split_http[5:base::length(split_http)], collapse = '/')
     folder_path <- base::gsub('%20', ' ', folder_path)
    
     # Extract the storage account name and container name from the HTTP link
     storage_account <- base::unlist(base::strsplit(split_http[3], "\\."))[1]
     container_name <- split_http[4]

     # Check extention
     file_ext <- tools::file_ext(http_link)
     if(file_ext =="parquet"){
          # Remove file name
          http_link <- base::dirname(http_link)
     }

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
     
     # Create a storage endpoint and container object
     blob_endpoint <- AzureStor::storage_endpoint(
          base::sprintf("https://%s.blob.core.windows.net",storage_account),
          sas=sas)
     container <- AzureStor::storage_container(
          blob_endpoint,
          container_name)

     # List all files in the specified folder path within the container
     files_list <- AzureStor::list_storage_files(
          container,
          folder_path)
     files_list <- files_list[files_list$isdir == FALSE, ]

     # Construct the full paths to the filtered files
     files_path <- base::lapply(
          files_list$name,function(path){
               base::paste0(
                    c(base::sprintf("https://%s.blob.core.windows.net",
                    storage_account),container_name, path),collapse = '/')})

     # Remove non .parquet links
     files_path <- files_path[grepl("\\.parquet$", files_path)]

     # Assign SAS
     for(i in 1:length(files_path)){
          file <- files_path[[i]]
          file <- paste0(file,"?",sas)
          files_path[[i]] <- file
     }

     # return
     return(files_path)
}
#
#' Internal function return_temp_path
#' @description 
#' Takes a in memory object, saves it to disk, and returns the path to temp file
#' @param x R object to save to disk
#' @return Character, path to temp file
return_temp_path <- function(x){
     # Check object type
     if(base::isa(x,"SpatRaster")) {
          r_path <- base::replicate(n = terra::nlyr(x), tempfile(fileext = '.tif'))
          terra::writeRaster(x, r_path)
     } else if(base::isa(x,c("sf","data.frame"))){
          r_path <- base::replicate(n = 1, tempfile(fileext = '.gpkg'))
          sf::st_write(x,r_path,quiet=TRUE)
     } else{
          cli::cli_abort("In memory object is neither a SpatRaster or sf.dataframe.")
     }

     # Return
     return(r_path)
}
#
#' Open geoparquet in DuckDB
#' @description 
#' Streams geoparquet data from blob into local DuckDB instance for querying
#' @param path_link Character, path/http_link to the parquet file to load
#' @param file_type Character, specify either "parquet" or "sf". All files from blob are parquet. sf is usefull for local files
#' @param link_expiration_time Double, duration of sas token for downloading (minutes)
#' @return DuckDB
#' @export
#' @import duckdbfs
#' @examples
#' \dontrun{
#' # Grab parquet link
#' aoi_metadata <- query_lookup(country="canada",subject = "geo-boundaries-adm1-simplified")
#' 
#' # Open dataset
#' aoi<-open_parquet(aoi_metadata$file_path)
#' 
#' # View dataset
#' dplyr::glimpse(aoi)
#' aoi |>
#'     dplyr::distinct(shapeISO) |> 
#'     dplyr::pull()
#' 
#' # Filter DB and create sf object
#' filter_aoi<-aoi |>
#'     dplyr::filter(shapeISO == "CA-QB") |>
#'     to_sf(crs = 4326)
#' 
#' # Plot
#' plot(duckdbfs::to_sf(aoi,crs=4326)$geom)
#' plot(filter_aoi$geom,add=TRUE,col="red")
#' }
open_parquet <- function(path_link,file_type="parquet",link_expiration_time=5) {
     # Prepare paths with SAS tokens
     if(base::isa(path_link,"character")){
          if (base::startsWith(path_link,"http")) {
               cli::cli_alert_info("Preparing link with SAS token ...")
               files_path <- prep_blob_paths(path_link,link_expiration_time)
          } else{
               files_path <- path_link       
          }
     } else{
          if(base::isa(path_link,c("sf","data.frame"))){
               files_path <- return_temp_path(path_link)
          } else{
               cli::cli_abort("In memory object is not a sf.dataframe.")
          }
     }

     # Create DB with spatial library
     duckdbfs::load_spatial()

     # Open dataset
     cli::cli_alert_info("Opening file in DuckDB ...")
     parquet <- duckdbfs::open_dataset(files_path,format=file_type)
     
     # Rename geometry
     column_names<-base::colnames(parquet)
     if("geometry" %in% column_names){
          parquet <- parquet |>
               dplyr::rename("geom"="geometry")
     }

     # Return
     cli::cli_alert_success("File opened.")
     return(parquet)
}
#
#' Spatial join in DuckDB
#' @description 
#' Uses SQL to perform spatial joins within DuckDB
#' @param db1 DuckDB, database opened using open_parquet()
#' @param db2 DuckDB, database opened using open_parquet() to used in spatial join
#' @param method Character, method for spatial join of http_link1 and http_link2. One of "st_intersects", "st_disjoint", "st_within", "st_dwithin", 
#'   "st_touches", "st_contains", "st_containsproperly", "st_covers", "st_overlaps", "st_equals", "st_crosses"
#' @param join_type Character, type of join. One of "left", "right", "inner", "full"
#' @return DuckDB
#' @export
#' @examples
#' \dontrun{
#'# Open large dataset you want to reduce spatially
#'http_link1 <- query_lookup(country="canada",subject = "geo-boundaries-adm1-simplified")
#'db1 <- open_parquet(
#'     http_link1$file_path,
#'     link_expiration_time=15)
#'
#'# Open smaller dataset used for spatial join
#'http_link2 <- query_lookup(country="canada",subject = "geo-boundaries-adm1-simplified")
#'db2 <- open_parquet(
#'     http_link2$file_path,
#'     link_expiration_time=15) |>
#'     dplyr::filter(shapeISO == "CA-QB")
#'
#'# Perform spatial join
#'db <- spatial_join_parquets(
#'     db1,
#'     db2,
#'     method="st_within",
#'     join_type="right") |>
#'     duckdbfs::to_sf(crs = 4326)
#'
#'# Plot
#'plot(duckdbfs::to_sf(db1,crs=4326)$geom)
#'plot(db$geom,add=TRUE,col="red")
#'
#'## Use local file for spatial join
#'# Open large dataset you want to reduce spatially
#'http_link1 <- query_lookup(country="canada",subject = "geo-boundaries-adm1-simplified")
#'db1 <- open_parquet(
#'     http_link1$file_path,
#'     link_expiration_time=15)
#'
#'# Open smaller dataset used for spatial join from local file system
#'http_link2 <- example_montreal_sf # local sf object
#'db2 <- open_parquet(
#'     http_link2,
#'     file_type="sf")
#'
#'# Perform spatial join
#'db <- spatial_join_parquets(
#'     db1,
#'     db2,
#'     method="st_intersects",
#'     join_type="right")|>
#'     duckdbfs::to_sf(crs = 4326)
#'
#'# Plot
#'plot(duckdbfs::to_sf(db1,crs=4326)$geom)
#'plot(db$geom,add=TRUE,col="red")
#' }
spatial_join_parquets <- function(db1,db2,method="st_intersects",join_type="right") {
     # Perform spatial join
     cli::cli_alert_info("Performing spatial join using {method} ...")
     spatial_join_parquet <- duckdbfs::spatial_join(
          db1,
          db2,
          by = method,
          join=join_type)
     
     # Remove other geometry
     spatial_join_parquet <- spatial_join_parquet |>
          dplyr::select(-"geom_1")
     
     # Return spatil joined parquet
     cli::cli_alert_success("Spatial join completed.")
     return(spatial_join_parquet)
}
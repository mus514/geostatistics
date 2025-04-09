
################################################################################
## Querying habitat's internal blob for http links to spatial assests 
################################################################################
#date: 2025-03-24
#authors: KJ Theron
#
#' Internal function rv_space_http
#' @description 
#' Removes white spaces in http links
#' @param df Dataframe, Habitat lookup table containing http links
#' @return Dataframe with white spaces removed in the http links
rv_space_http <- function(df){
     # Loop over rows in dataframe
     for(i in 1:base::nrow(df)){
          # Replace white spaces
          df[i,]$file_path <- base::gsub(" ", "%20", df[i,]$file_path)
     }
     # Return
     base::return(df)
}
#
#' Query habitat's lookup table
#' @description 
#' Uses the Habitat Blob storage lookup table and filters the rows to grab an asset link
#' @param complet_file_name Character, string to filter the file name of the assets.
#' @param theme Character, string to filter the theme of the assets. (e.g. hydrology, biodiversity)
#' @param country Character, string to filter the country of the assets. (e.g. global, canada, america)
#' @param region Character, string to filter the region code of the assets. (e.g. all, qc)
#' @param subject Character, name of the file on the blob
#' @param year Numeric, number to filter the year of the assets. (e.g. 2023)
#' @param version Character, string to filter the version of the assets. (e.g. v0)
#' @param view_catalog Boolean, to return the names of the assets or not
#' @return Dataframe, containing a single row with the filtered assests metadata
#' @export
#' @examples
#' \dontrun{
#' library(dplyr)
#' geoboundaries_France <- query_lookup(country="france",subject = "geo-boundaries-adm1-simplified")
#' geoboundaries_France$file_path
#' }
query_lookup <- function(complet_file_name=NULL,theme=NULL,country=NULL,region=NULL,subject=NULL,year=NULL,version=NULL,view_catalog=FALSE){
     # Check if account_name is set
     check_account_name()

     # Path to lookup table
     account_name <- base::Sys.getenv("ACCOUNT_NAME")
     look_up<-paste0("https://",account_name,".blob.core.windows.net/habitat/Tables/lookup_table/geospatial/lookup_table.csv")

     # Assign SAS
     sas_token <- base::Sys.getenv("SAS_TOKEN")
     if(sas_token==""){
          look_up <- create_sas(look_up,sas_duration=1,retun_link_with_SAS=TRUE)
     } else{
          # Is SAS valid?
          sas_validity <- is.sas.valid(sas_token)
          if(sas_validity[[1]] < 0) {
               cli::cli_abort("The sas token is NOT valid. Please create a new one.")
          } else{
               look_up <- base::paste0(look_up,"?",sas_token)
          }          
     }
     
     # Load csv file with blob links
     data_look_up<-utils::read.csv(look_up)

     # Clean links
     data_look_up<-rv_space_http(data_look_up)

     # Return the lookup table
     if(view_catalog==TRUE){
          base::return(data_look_up)
     }

     # Filter lookup table to find asset
     if(!base::is.null(complet_file_name)){
          data_look_up<-dplyr::filter(data_look_up,complet_file_name==!!complet_file_name) 
          if(base::nrow(data_look_up)==0){base::stop("Please check the complet_file_name provided for filtering.")}}
     if(!base::is.null(theme)){
          data_look_up<-dplyr::filter(data_look_up,theme==!!theme) 
          if(base::nrow(data_look_up)==0){base::stop("Please check the theme name provided for filtering.")}}
     if(!base::is.null(country)){
          data_look_up<-dplyr::filter(data_look_up,country==!!country) 
          if(base::nrow(data_look_up)==0){base::stop("Please check the country name provided for filtering.")}}
     if(!base::is.null(region)){
          data_look_up<-dplyr::filter(data_look_up,region==!!region) 
          if(base::nrow(data_look_up)==0){base::stop("Please check the region name provided for filtering.")}}
     if(!base::is.null(subject)){
          data_look_up<-dplyr::filter(data_look_up,subject==!!subject) 
          if(base::nrow(data_look_up)==0){base::stop("Please check the subject name provided for filtering.")}}
     if(!base::is.null(year)){
          data_look_up<-dplyr::filter(data_look_up,year==!!year) 
          if(base::nrow(data_look_up)==0){base::stop("Please check the year provided for filtering.")}}
     if(!base::is.null(version)){
          data_look_up<-dplyr::filter(data_look_up,version==!!version) 
          if(base::nrow(data_look_up)==0){base::stop("Please check the version provided for filtering.")}}

     # Return
     base::return(data_look_up)
}
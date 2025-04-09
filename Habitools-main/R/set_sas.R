
################################################################################
## Function to create a SAS token
################################################################################
#date: 2025-03-24
#authors: Mustapha Bouhsen, KJ Theron
#
#' Internal function prepare_http_link
#' @description
#' Uses a http link to an asset on the blob to identify the storage account
#' @param link_path Character, the http_link/path to file
#' @return List, name of the account, and path to file
prepare_http_link <- function(link_path){
     # Split link
     split_http <- base::unlist(base::strsplit(link_path, "/"))

     # Grab account name
     storage_account <- base::unlist(base::strsplit(split_http[3], "\\."))[1]

     # Grab container name
     container_name <- split_http[4]

     # Re-create path to file
     split_http <- split_http[-(1:4)]
     path <- base::do.call(base::file.path, as.list(split_http))

     # Return
     return(base::list(storage_account,container_name,path))
}
# 
#' Internal function check_az_installed
#' @description 
#' Function that calls az on the command line to see if its installed
#' @return Error message if az is not found or installed
check_az_installed <- function(){
     # Check is az can be called
     result <- tryCatch({
          test <- base::system("az --version", intern = TRUE)}, error = function(e) {
               cli::cli_alert_danger("AZ can not be found or is not installed!")
               base::message(
                    base::paste0(
                         "  Please install AZ and update it. In powershell (Windows) run:",
                         "\n  winget install --exact --id Microsoft.AzureCLI",
                         "\n  Add this path to your environmental variables: C:\\Program Files\\Microsoft SDKs\\Azure\\CLI2\\wbin",
                         "\n  az upgrade",
                         "\n  az login",
                         "\n\n  In terminal (Linux) run:",
                         "\n  curl -sL https://aka.ms/InstallAzureCLIDeb | sudo bash",
                         "\n  az upgrade",
                         "\n  az login"
                    )
               )
               base::stop("check_az_installed()",call.=FALSE)
          }
     )
}
# 
#' Internal function check_account_name
#' @description 
#' Function to check if the Habitat blob account name is present in .Renviron file
#' @return Error message if account name is not set
check_account_name <- function(){
     # See if account name has been set
     account_name<-base::Sys.getenv("ACCOUNT_NAME")
     if(account_name==""){
          cli::cli_alert_danger("The Habitat Blob account name has not been set.")
          base::message(
               base::paste0(
                    "  Pleae contact the Data Manager at Habitat to obtain the account name, them run:",
                    "\n  usethis::edit_r_environ()",
                    "\n\n  Copy paste the following in it:",
                    "\n  ACCOUNT_NAME=the_name_given_by_Data_Manager",
                    "\n\n  Remember to restart R terminal!"
               )
          )
          base::stop("check_account_name()",call.=FALSE)
     }
}
#
#' Internal function create_sas
#' @description 
#' Create a SAS token to be pasted inside your .Renviron
#' @param link_path Character, the http_link/path to file
#' @param sas_duration Double, number indicating the duration the SAS should allow access in minutes.
#' @param retun_link_with_SAS Boolean, whether to return the SAS token along, or appended with http link
#' @param verbose Boolean, to return full terminal output or hide it
#' @return Character, sas token
#' @export
#' @examples
#' \dontrun{
#' # Create account sas
#' create_sas()
#' }
create_sas <- function(link_path=NULL,sas_duration=180,retun_link_with_SAS=FALSE,verbose=FALSE){
     # See if AZ is installed
     check_az_installed()

     # Check if account_name is set
     check_account_name()

     # Set the expiry time for the SAS token (2 minutes from now)
     expiry_time <- base::Sys.time()
     expiry_time <- expiry_time + base::as.difftime(4, units = 'hours') # add 4 hours to match azure local time
     expiry_time <- expiry_time + base::as.difftime(sas_duration, units = 'mins')
     expiry_time <- base::format(expiry_time, "%Y-%m-%dT%H:%M:%SZ")

     # Create account sas, else sas per file
     if(base::is.null(link_path)){
          # Command
          command <- base::paste(
               "az storage account generate-sas",
               "--account-name", base::Sys.getenv("ACCOUNT_NAME"),
               "--permissions rl",
               "--resource-types sco",
               "--services b",
               "--expiry", expiry_time,
               "--https-only"
          )

          # Create sas
          if(verbose==TRUE){
               sas_token <- base::system(command, intern = TRUE)
               
          } else{
               sas_token <- base::system(command, intern = TRUE, ignore.stdout = FALSE, ignore.stderr = TRUE)
          }

          # Remove any double quotes from the SAS token
          sas_token <- base::gsub('"', "", sas_token)

          # Return
          return(sas_token)
     } else{
          # Grab acount name and item path from link
          link_info <- prepare_http_link(link_path)
          account_name <- link_info[[1]]
          container_name <- link_info[[2]]
          file_path <- link_info[[3]]

          # Generate a SAS token for the specified path
          command <- base::paste(
               "az storage blob generate-sas",
               "--account-name", account_name,
               "--container-name", container_name,
               "--name", file_path,
               "--permissions r",
               "--expiry", expiry_time,
               "--https-only")

          # Execute the Azure CLI command and capture the output
          if(verbose==TRUE){
               sas_token <- base::system(command, intern = TRUE)
               
          } else{
               sas_token <- base::system(command, intern = TRUE, ignore.stdout = FALSE, ignore.stderr = TRUE)
          }

          # Remove any double quotes from the SAS token
          sas_token <- base::gsub('"', "", sas_token)

          # Return
          if(retun_link_with_SAS==TRUE){
               sas_token <- base::paste0(link_path,"?",sas_token)
          }
          return(sas_token)
     }
}
#
#' Internal function is.sas.valid
#' @description 
#' Checks if the sas token in the .Renviron is valid
#' @param sas_token Character, sas token
#' @return Character, sas token
is.sas.valid <- function(sas_token){
     # Extract date from SAS token
     current_date <- base::Sys.time()
     end_date_str <- base::sub(".*se=([^&]+)&.*", "\\1", sas_token)
     end_date_str <- utils::URLdecode(end_date_str)

     # Convert the strings to date-time objects
     end_date <- base::as.POSIXct(end_date_str, format="%Y-%m-%dT%H:%M:%SZ", tz="UTC")

     # Calculate the time difference
     time_difference_sec <- base::as.numeric(base::difftime(end_date, current_date, units="secs"))

     # Calculate the remaining time in days, hours, and minutes
     days_remaining <- base::floor(time_difference_sec / (24 * 3600))
     hours_remaining <- base::floor((time_difference_sec %% (24 * 3600)) / 3600)
     minutes_remaining <- base::floor((time_difference_sec %% 3600) / 60)

     # Return
     return(base::list(time_difference_sec,days_remaining,hours_remaining,minutes_remaining))
}
#
#' Create SAS for .Renviron
#' @description 
#' Create a SAS token to be pasted inside your .Renviron
#' @param sas_duration Double, number indicating the duration the SAS should allow access in minutes.
#' @param update Boolean, whether to update the sas token.
#' @param verbose Boolean, to return full terminal output or hide it.
#' @return Character, messaging to update or check validity of sas in .Renviron
#' @export 
#' @examples
#' \dontrun{
#' # Create sas for .Renviron
#'set_sas(sas_duration=180)
#'
#'# Update sas for .Renviron
#'set_sas(sas_duration=180,update=TRUE)
#' }
set_sas <- function(sas_duration=180,update=FALSE,verbose=FALSE){
     # See if sas token has been set
     sas_token <- base::Sys.getenv("SAS_TOKEN")
     if(sas_token==""){
          cli::cli_alert_info("No SAS token present in .Renviron, creating new one for {sas_duration} minutes...")

          # Create sas
          sas_token <- create_sas(sas_duration=sas_duration)

          # Print
          cli::cli_alert_success("SAS token created successfully.")  
          base::message(
               base::paste0(
                    "  Run:",
                    "\n  usethis::edit_r_environ()",
                    "\n\n  Copy paste the following in it:",
                    "\n  SAS_TOKEN=",sas_token,
                    "\n\n  Remember to restart R terminal!"
               )
          )
     } else if(update==FALSE){
          cli::cli_alert_info("SAS token found, checking if it's still valid...")

          # Is SAS valid?
          sas_validity <- is.sas.valid(sas_token)

          # If expired, create new one
          if(sas_validity[[1]] < 0) {
               cli::cli_alert_info("The sas token is NOT valid. Creating new sas...")

               # Create sas
               sas_token <- create_sas(sas_duration=sas_duration)

               # Print
               cli::cli_alert_success("SAS token created successfully.")  
               return({
                    base::message(
                         base::paste0(
                              "  Run:",
                              "\n  usethis::edit_r_environ()",
                              "\n\n  Copy paste the following in it:",
                              "\n  SAS_TOKEN=",sas_token,
                              "\n\n  Remember to restart R terminal!"
                         )
                    )
               })
          } else{          
               # Print validity
               cli::cli_alert_success("SAS token is still valid for {sas_validity[[2]]} days, {sas_validity[[3]]} hours, and {sas_validity[[4]]} minutes.")
          }
     } else{
          cli::cli_alert_info("Updating sas token...")

          # Create sas
          sas_token <- create_sas(sas_duration=sas_duration)

          # Print
          cli::cli_alert_success("SAS token created successfully.")  
          return({
               base::message(
                    base::paste0(
                         "  Run:",
                         "\n  usethis::edit_r_environ()",
                         "\n\n  Copy paste the following in it:",
                         "\n  SAS_TOKEN=",sas_token,
                         "\n\n  Remember to restart R terminal!"
                    )
               )
          })
     }
}
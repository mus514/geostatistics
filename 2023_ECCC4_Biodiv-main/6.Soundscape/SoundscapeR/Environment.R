####PACKAGES####
library(soundecology)
# devtools::install_github(repo = "ThomasLuypaert/soundscapeR")
library(soundscapeR)
library(tuneR)
library(dplyr)
library(signal) # signal processing functions
library(oce) # image plotting functions and nice color maps
library(sound)
library(stringr)

####PATHS####
pathAudio <- "~/Habitat/Soundscape/audio/"


####FUNCTIONS####
ss_assess_files_new <- function(file_locs, full_days = TRUE) {
  # 1. Find the sampling regime of each folder (median timeinterval between sound files)
  
  sample_regime <- vector("list")
  
  for (i in 1:length(file_locs)) {
    regime <- sapply(
      file_locs[[i]],
      function(x) sub(".*_(\\d{8}_\\d{6})\\.WAV", "\\1", x)
    )
    
    regime <- diff(unlist(lapply(regime, function(x) {
      as.POSIXct(strptime(x,
                          "%Y%m%d_%H%M%S",
                          tz = "America/Manaus"
      ))
    }))) / 60
    
    median_regime <- stats::median(regime)
    
    sample_regime[[i]] <- regime
    
    # 2. Check if any of the files deviate from the expected sampling regime
    # For instance: are some files missing from the expected sequence
    
    if (any(regime > median_regime)) {
      outliers <- c()
      
      for (j in 2:length(regime)) {
        if (as.numeric(regime[j] - regime[j - 1], units = "secs") > median_regime) {
          outliers <- c(outliers, j)
        }
      }
      
      fileloc_names <- names(file_locs)[i]
      
      cli::cli_alert_danger("Irregular timeinterval detected for: {fileloc_names}")
      cli::cli_alert_info("Based on the expected sampling regime, there are missing files...")
      
      missing_files <- basename(file_locs[[i]][outliers])
      
      cli::cli_alert("{missing_files}")
      
      stop("Irregular timeintervals detected - check files")
    } else {
      # 3. If no missing files, assess how many full sampling days per site
      # Subset the fileloc list to contain only full days
      
      files_per_day <- 1440 / as.numeric(median_regime)
    }
  }
  
  if (full_days == TRUE) {
    subset_to_closest_multiple <- function(my_list, multiple) {
      # Create a new list to store the subsetted vectors
      my_list_subset <- vector("list", length(my_list))
      
      # Loop over each vector in the list
      for (i in seq_along(my_list)) {
        # Check if the length of the vector is a multiple of 288
        if (length(my_list[[i]]) %% multiple == 0) {
          my_list_subset[[i]] <- my_list[[i]] # No need to subset
        } else {
          # Subset the vector to the closest multiple of 288
          num_to_keep <- length(my_list[[i]]) %/% multiple * multiple
          my_list_subset[[i]] <- my_list[[i]][1:num_to_keep]
        }
      }
      
      return(my_list_subset)
    }
    
    subset_list <- subset_to_closest_multiple(my_list = file_locs, multiple = files_per_day)
    
    cli::cli_alert_info("Sampling regime for study: 1 minute in {median_regime} minutes")
    cli::cli_alert_info("Number of files per day: {files_per_day} files")
    cli::cli_alert_info("The file_locs argument has been subsetted to contain only full sampling days")
    
    return(subset_list)
  } else {
    cli::cli_alert_info("Sampling regime for study: 1 minute in {median_regime} minutes")
    cli::cli_alert_info("Number of files per day: {files_per_day} files")
    cli::cli_alert_warning("The data still contains partially sampled days - be careful with downstream analysis...")
    
    return(file_locs)
  }
}


extract_time <- function(path) {
  date <- str_extract(path, "(?<=_)[0-9]{8}(?=_)")
  moment <- str_extract(path, "\\d{6}(?=\\.WAV$)") # Extract the time part
  time_str <- paste0(date, moment)
  time <- as.POSIXct(time_str, format = "%Y%m%d%H%M%S", tz = "UTC")
  return(time)
}

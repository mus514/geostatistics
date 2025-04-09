###########################
## Calculate indices
##########################
## Date initiale: 24 avril 2024
## Author : Noemie Lacroix-D, Mederic Durand

####PACKAGES####
library(lubridate) # managing dates
library(soundecology) #multiple_sounds, parallel, ndsi...
library(soundscapeR)
library(tuneR)
library(dplyr)
library(signal) # signal processing functions
library(oce) # image plotting functions and nice color maps
library(sound)
library(stringr)
library(seewave)
library(parallel)
library(ggplot2)
library(gridExtra)
library(readr)
library(patchwork)
library(ggbreak)


####NAME SPACE####
# Specifically naming the imports from soundecology package, in order to ease 
# the transfer of the function to this script
# More info here: https://github.com/ljvillanueva/soundecology/tree/master/soundecology/R
# importFrom("graphics", "lines", "locator", "rect")
# importFrom("utils", "write.table")
# importFrom("pracma", trapz)
# importFrom("oce", pwelch)
# importFrom("ineq", Gini)
# importFrom("vegan", diversity)
# importFrom("tuneR", channel, readWave)
# importFrom("seewave", spectro, meandB, dfreq, wav2flac, H, cutw)
# importFrom("parallel", makeCluster, parLapply, detectCores, stopCluster)
# exportPattern("^[[:alpha:]]+")

####FUNCTIONS####

assert_non_empty <- function(toAssert){
       files <- list.files(toAssert, full.names = TRUE)

       for (file in files){
              file_size <- file.info(file)$size

              if (file_size == 0) {
                     stop("File ", file, " is 0KB\n")
              }
       }
}

load_daily <- function(path, site){

       # This function parses through the recordings of an audiomoth folder,
       # extracts the dates of each recordings within the folder and their respective folder names
       # and returns it as a dataframe

       foldernames <- list.dirs(path, recursive = FALSE) # get list of daily subfolders

       nrows <- length(foldernames)
       
       df <- data.frame(site = rep(site, nrows), date = rep(NA, nrows), foldername = rep(NA, nrows)) # generate df
       df$foldername <- foldernames # append to df

       i <- 1
       for (folder in foldernames){
              df$date_raw[i] <- substr(folder[1], nchar(folder[1])-7, nchar(folder[1])) #Extract date from foldername
              # assert_non_empty(toAssert = folder) # Add sanity check
              i <- i+1
       }

       if(rlang::is_empty(df$date_raw)){  # Add sanity check
              stop(paste0("Empty DFday dataframe for ", path,
              ". Is the data folder structure correct?"))
       }

       df <- df %>%
              mutate(
                     date = lubridate::ymd(date_raw),
                     year = year(date),
                     month = month(date),
                     day = day(date)                     
              )
   
       df <- df %>% select(site, date, year, month, day, foldername,date_raw)

       return(df)
}

load_site <- function(site="All", AudioIndex, path){

    if(site == "All"){site <- ""} # Load every site in folder if set to "All"

    results_list <- list.files(path = path,
        pattern = paste0("^", site,".*", AudioIndex, "\\.csv$"),
        full.names = TRUE)

    results <- results_list %>%
        lapply(read_csv) %>%
        bind_rows()

    return(results)
}


processOutput <- function(df = data.frame, index = character, path = character, milieu = character){

       # This function loads the soundecology outputs for a given index
       # and binds it in a single dataframe for each site
       # It adds a time column inferred from the recording filename
       # and tiddies up the data

       #### TO DO:
       #### Are all cols the same for each index?
       ####
       #### If yes, then rename the LEFT_CHANNEL column and Concatenate?

       filenames <- Sys.glob(paste0(path, "*_", index, ".csv")) # Grab filenames
       tables <- lapply(filenames, read.csv, header = TRUE) # Load them
       results <- do.call(rbind, tables) # Bind them in a single dataframe

       #### ADD TIME DATA ####
       results <- results %>%
              mutate(date_raw = substr(FILENAME, 1, 8)) %>%
              left_join(DFday, by = "date_raw") %>% # Join with DFday dataframe
              mutate(moment = str_extract(FILENAME, "\\d{6}(?=\\.WAV$)")) %>% # Extract time of day
              mutate(date_time = as.numeric(paste0(date_raw, moment))) %>% # Concatenate with day of recording
              mutate(moment = as.numeric(moment)) %>%
              mutate(time = lubridate::ymd_hms(date_time), tz = "UTC") %>% # Transform in date format
              mutate(AM_PM = ifelse(moment < 120000, "AM", "PM")) %>% # Assign AM or PM
              mutate(site_type = milieu) %>%
              mutate(time = format(time, "%H:%M:%S")) %>%
              select(-c(RIGHT_CHANNEL, FILENAME, tz)) %>% # discard these columns (Right channel = we record in mono, tz = time zone, FILENAME = duplicated)
              relocate(foldername, site, date, time, year, month, 
                     day, site_type, INDEX, LEFT_CHANNEL, .before = 1) # Tiddy first few cols, the last columns are index-dependant

       return(results)
}




multiple_indices <- function (directory, resultfile, soundindex = c("ndsi", "acoustic_complexity", 
    "acoustic_diversity", "acoustic_evenness", "bioacoustic_index", 
    "H"), no_cores = 1, flac = FALSE, from = NA, to = NA, units = NA, 
    empty = NA, ...) 
{

    # Ripped from soundecology package
    # but added an exception for empty soundfiles
    #print(empty)

    if (any(soundindex %in% c("ndsi", "acoustic_complexity", 
        "acoustic_diversity", "acoustic_evenness", "bioacoustic_index", 
        "H")) == FALSE) {
        stop(paste("Unknown function", soundindex))
    }
    if (file.access(directory) == -1) {
        stop(paste("The directory specified does not exist or this user is not autorized to read it:\n    ", 
            directory))
    }
    if (is.na(from) == FALSE) {
        if (is.na(to) || is.na(units)) {
            stop("All three arguments 'from', 'to', and 'units' must be specified.")
        }
    }
    if (is.na(to) == FALSE) {
        if (is.na(from) || is.na(units)) {
            stop("All three arguments 'from', 'to', and 'units' must be specified.")
        }
    }
    if (is.na(units) == FALSE) {
        if (is.na(from) || is.na(to)) {
            stop("All three arguments 'from', 'to', and 'units' must be specified.")
        }
    }
    thismachine_cores <- detectCores()
    if (no_cores == 0) {
        stop("Number of cores can not be 0.")
    }
    else if (no_cores < -1) {
        stop("Number of cores can not be negative.")
    }
    else if (no_cores == "max") {
        no_cores = thismachine_cores
    }
    else if (no_cores == -1) {
        no_cores = thismachine_cores - 1
    }
    else if (no_cores > thismachine_cores) {
        warning(paste(" The number of cores to use can not be more than the\n\t\t\t\t\t  cores in this computer: ", 
            detectCores()), immediate. = TRUE)
        no_cores <- thismachine_cores
    }
    if (flac == TRUE) {
        wav_files <- dir(path = directory, pattern = "flac$", 
            ignore.case = TRUE)
        if (length(wav_files) == 0) {
            stop(paste("Could not find any .flac files in the specified directory:\n    ", 
                directory))
        }
    }
    else {
        wav_files <- dir(path = directory, pattern = "wav$", 
            ignore.case = TRUE)
        if (length(wav_files) == 0) {
            stop(paste("Could not find any .wav files in the specified directory:\n    ", 
                directory))
        }
    }
    get_wav <- function(directory, flacsoundfile) {
        if (.Platform$OS.type == "windows") {
            from_file = paste(directory, "\\", flacsoundfile, 
                sep = "")
        }
        else {
            from_file = paste(directory, "/", flacsoundfile, 
                sep = "")
        }
        wav_file = paste(strtrim(flacsoundfile, nchar(flacsoundfile) - 
            5), "wav", sep = ".")
        file.copy(from = from_file, to = flacsoundfile)
        wav2flac(flacsoundfile, reverse = TRUE, overwrite = TRUE)
        file.remove(flacsoundfile)
        if (file.exists(wav_file)) {
            return(wav_file)
        }
        else {
            return(NA)
        }
    }
    if (soundindex == "bioacoustic_index") {
        fileheader <- c("FILENAME,SAMPLINGRATE,BIT,DURATION,CHANNELS,INDEX,FFT_W,MIN_FREQ,MAX_FREQ,LEFT_CHANNEL,RIGHT_CHANNEL")
        getindex <- function(soundfile, inCluster = FALSE, ...) {
            if (inCluster == TRUE) {
                require(soundecology)
            }
            args <- list(...)
            if (!is.null(args[["min_freq"]])) {
                min_freq = args[["min_freq"]]
            }
            else {
                min_freq = formals(bioacoustic_index)$min_freq
            }
            if (!is.null(args[["max_freq"]])) {
                max_freq = args[["max_freq"]]
            }
            else {
                max_freq = formals(bioacoustic_index)$max_freq
            }
            if (!is.null(args[["fft_w"]])) {
                fft_w = args[["fft_w"]]
            }
            else {
                fft_w = formals(bioacoustic_index)$fft_w
            }
            if (flac == TRUE) {
                soundfile_path <- get_wav(directory, soundfile)
            }
            else {
                if (.Platform$OS.type == "windows") {
                  soundfile_path = paste(directory, "\\", soundfile, 
                    sep = "")
                }
                else {
                  soundfile_path = paste(directory, "/", soundfile, 
                    sep = "")
                }
            }
            if (is.na(from) == FALSE) {
                this_soundfile <- tuneR::readWave(soundfile_path, from = from, 
                  to = to, units = units)
            }
            else {
                this_soundfile <- tuneR::readWave(soundfile_path)
            }
            return_list <- bioacoustic_index(this_soundfile, 
                ...)
            if (this_soundfile@stereo == TRUE) {
                no_channels = 2
            }
            else {
                no_channels = 1
            }
            if (flac == TRUE) {
                file.remove(soundfile_path)
            }
            return(paste("\n", soundfile, ",", this_soundfile@samp.rate, 
                ",", this_soundfile@bit, ",", round(length(this_soundfile@left)/this_soundfile@samp.rate, 
                  2), ",", no_channels, ",", soundindex, ",", 
                fft_w, ",", min_freq, ",", max_freq, ",", return_list$left_area, 
                ",", return_list$right_area, sep = ""))
        }
    }
    else if (soundindex == "acoustic_diversity") {
        fileheader <- c("FILENAME,SAMPLINGRATE,BIT,DURATION,CHANNELS,INDEX,MAX_FREQ,DB_THRESHOLD,FREQ_STEPS,LEFT_CHANNEL,RIGHT_CHANNEL")
        getindex <- function(soundfile, inCluster = FALSE, ...) {
            if (inCluster == TRUE) {
                require(soundecology)
            }
            args <- list(...)
            if (!is.null(args[["db_threshold"]])) {
                db_threshold = args[["db_threshold"]]
            }
            else {
                db_threshold = formals(acoustic_diversity)$db_threshold
            }
            if (!is.null(args[["max_freq"]])) {
                max_freq = args[["max_freq"]]
            }
            else {
                max_freq = formals(acoustic_diversity)$max_freq
            }
            if (!is.null(args[["freq_step"]])) {
                freq_step = args[["freq_step"]]
            }
            else {
                freq_step = formals(acoustic_diversity)$freq_step
            }
            if (flac == TRUE) {
                soundfile_path <- get_wav(directory, soundfile)
            }
            else {
                if (.Platform$OS.type == "windows") {
                  soundfile_path = paste(directory, "\\", soundfile, 
                    sep = "")
                }
                else {
                  soundfile_path = paste(directory, "/", soundfile, 
                    sep = "")
                }
            }
            if (is.na(from) == FALSE) {
                this_soundfile <- tuneR::readWave(soundfile_path, from = from, 
                  to = to, units = units)
            }
            else {
                this_soundfile <- tuneR::readWave(soundfile_path)
            }
            return_list <- acoustic_diversity(this_soundfile, 
                ...)
            if (this_soundfile@stereo == TRUE) {
                no_channels = 2
            }
            else {
                no_channels = 1
            }
            if (flac == TRUE) {
                file.remove(soundfile_path)
            }
            return(paste("\n", soundfile, ",", this_soundfile@samp.rate, 
                ",", this_soundfile@bit, ",", round(length(this_soundfile@left)/this_soundfile@samp.rate, 
                  2), ",", no_channels, ",", soundindex, ",", 
                max_freq, ",", db_threshold, ",", freq_step, 
                ",", return_list$adi_left, ",", return_list$adi_right, 
                sep = ""))
        }
    }
    else if (soundindex == "acoustic_complexity") {
        fileheader <- c("FILENAME,SAMPLINGRATE,BIT,DURATION,CHANNELS,INDEX,FFT_W,MIN_FREQ,MAX_FREQ,J,LEFT_CHANNEL,RIGHT_CHANNEL")
        getindex <- function(soundfile, inCluster = FALSE, ...) {
            if (inCluster == TRUE) {
                require(soundecology)
            }
            args <- list(...)
            if (!is.null(args[["max_freq"]])) {
                max_freq = args[["max_freq"]]
            }
            else {
                max_freq = formals(soundecology::acoustic_complexity)$max_freq
            }
            if (!is.null(args[["min_freq"]])) {
                min_freq = args[["min_freq"]]
            }
            else {
                min_freq = 1
            }
            if (!is.null(args[["j"]])) {
                j = args[["j"]]
            }
            else {
                j = formals(soundecology::acoustic_complexity)$j
            }
            if (!is.null(args[["fft_w"]])) {
                fft_w = args[["fft_w"]]
            }
            else {
                fft_w = formals(soundecology::acoustic_complexity)$fft_w
            }
            if (flac == TRUE) {
                soundfile_path <- get_wav(directory, soundfile)
            }
            else {
                if (.Platform$OS.type == "windows") {
                  soundfile_path = paste(directory, "\\", soundfile, 
                    sep = "")
                }
                else {
                  soundfile_path = paste(directory, "/", soundfile, 
                    sep = "")
                }
            }
            if (is.na(from) == FALSE) {
                this_soundfile <- tuneR::readWave(soundfile_path, from = from, 
                  to = to, units = units)
            }
            else {
                this_soundfile <- tuneR::readWave(soundfile_path)
            }
            return_list <- soundecology::acoustic_complexity(this_soundfile, 
                ...)
            if (this_soundfile@stereo == TRUE) {
                no_channels = 2
            }
            else {
                no_channels = 1
            }
            if (flac == TRUE) {
                file.remove(soundfile_path)
            }
            return(paste("\n", soundfile, ",", this_soundfile@samp.rate, 
                ",", this_soundfile@bit, ",", round(length(this_soundfile@left)/this_soundfile@samp.rate, 
                  2), ",", no_channels, ",", soundindex, ",", 
                fft_w, ",", min_freq, ",", max_freq, ",", j, 
                ",", return_list$AciTotAll_left, ",", return_list$AciTotAll_right, 
                sep = ""))
        }
    }
    else if (soundindex == "ndsi") {
        fileheader <- c("FILENAME,SAMPLINGRATE,BIT,DURATION,CHANNELS,INDEX,FFT_W,ANTHRO_MIN,ANTHRO_MAX,BIO_MIN,BIO_MAX,LEFT_CHANNEL,RIGHT_CHANNEL")
        getindex <- function(soundfile, inCluster = FALSE, ...) {
            if (inCluster == TRUE) {
                require(soundecology)
            }
            args <- list(...)
            if (!is.null(args[["fft_w"]])) {
                fft_w = args[["fft_w"]]
            }
            else {
                fft_w = formals(soundecology::ndsi)$fft_w
            }
            if (!is.null(args[["anthro_min"]])) {
                anthro_min = args[["anthro_min"]]
            }
            else {
                anthro_min = formals(soundecology::ndsi)$anthro_min
            }
            if (!is.null(args[["anthro_max"]])) {
                anthro_max = args[["anthro_max"]]
            }
            else {
                anthro_max = formals(soundecology::ndsi)$anthro_max
            }
            if (!is.null(args[["bio_min"]])) {
                bio_min = args[["bio_min"]]
            }
            else {
                bio_min = formals(soundecology::ndsi)$bio_min
            }
            if (!is.null(args[["bio_max"]])) {
                bio_max = args[["bio_max"]]
            }
            else {
                bio_max = formals(soundecology::ndsi)$bio_max
            }
            if (flac == TRUE) {
                soundfile_path <- get_wav(directory, soundfile)
            }
            else {
                if (.Platform$OS.type == "windows") {
                  soundfile_path = paste(directory, "\\", soundfile, 
                    sep = "")
                }
                else {
                  soundfile_path = paste(directory, "/", soundfile, 
                    sep = "")
                }
            }
            if (is.na(from) == FALSE) {
                this_soundfile <- tuneR::readWave(soundfile_path, from = from, 
                  to = to, units = units)
            }
            else {
                this_soundfile <- tuneR::readWave(soundfile_path)
            }
            return_list <- soundecology::ndsi(this_soundfile, ...)
            if (this_soundfile@stereo == TRUE) {
                no_channels = 2
            }
            else {
                no_channels = 1
            }
            if (flac == TRUE) {
                file.remove(soundfile_path)
            }
            return(paste("\n", soundfile, ",", this_soundfile@samp.rate, 
                ",", this_soundfile@bit, ",", round(length(this_soundfile@left)/this_soundfile@samp.rate, 
                  2), ",", no_channels, ",", soundindex, ",", 
                fft_w, ",", anthro_min, ",", anthro_max, ",", 
                bio_min, ",", bio_max, ",", return_list$ndsi_left, 
                ",", return_list$ndsi_right, sep = ""))
        }
    }
    else if (soundindex == "H") {
        fileheader <- c("FILENAME,SAMPLINGRATE,BIT,DURATION,CHANNELS,INDEX,WL,ENVT,MSMOOTH,KSMOOTH,LEFT_CHANNEL,RIGHT_CHANNEL")
        getindex <- function(soundfile, inCluster = FALSE, ...) {
            if (inCluster == TRUE) {
                require(soundecology)
            }
            args <- list(...)
            if (!is.null(args[["wl"]])) {
                wl = args[["wl"]]
            }
            else {
                wl = formals(seewave::H)$wl
            }
            if (!is.null(args[["envt"]])) {
                envt = args[["envt"]]
            }
            else {
                envt = formals(seewave::H)$envt
            }
            if (!is.null(args[["msmooth"]])) {
                msmooth = args[["msmooth"]]
            }
            else {
                msmooth = formals(seewave::H)$msmooth
                if (is.null(msmooth)) {
                  msmooth = "NULL"
                }
            }
            if (!is.null(args[["ksmooth"]])) {
                ksmooth = args[["ksmooth"]]
            }
            else {
                ksmooth = formals(seewave::H)$ksmooth
                if (is.null(ksmooth)) {
                  ksmooth = "NULL"
                }
            }
            if (flac == TRUE) {
                soundfile_path <- get_wav(directory, soundfile)
            }
            else {
                if (.Platform$OS.type == "windows") {
                  soundfile_path = paste(directory, "\\", soundfile, 
                    sep = "")
                }
                else {
                  soundfile_path = paste(directory, "/", soundfile, 
                    sep = "")
                }
            }
            if (is.na(from) == FALSE) {
                this_soundfile <- tuneR::readWave(soundfile_path, from = from, 
                  to = to, units = units)
            }
            else {
                this_soundfile <- tuneR::readWave(soundfile_path)
            }
            if (this_soundfile@stereo == TRUE) {
                left <- tuneR::channel(this_soundfile, which = c("left"))
                right <- tuneR::channel(this_soundfile, which = c("right"))
                left_res <- seewave::H(left, ...)
                right_res <- seewave::H(right, ...)
            }
            else {
                left <- tuneR::channel(this_soundfile, which = c("left"))
                left_res <- seewave::H(left, ...)
                right_res <- NA
            }
            if (this_soundfile@stereo == TRUE) {
                no_channels = 2
            }
            else {
                no_channels = 1
            }
            if (flac == TRUE) {
                file.remove(soundfile_path)
            }
            return(paste("\n", soundfile, ",", this_soundfile@samp.rate, 
                ",", this_soundfile@bit, ",", round(length(this_soundfile@left)/this_soundfile@samp.rate, 
                  2), ",", no_channels, ",", soundindex, ",", 
                wl, ",", envt, ",", msmooth, ",", ksmooth, ",", 
                left_res, ",", right_res, sep = ""))
        }
    }
    else if (soundindex == "acoustic_evenness") {
        fileheader <- c("FILENAME,SAMPLINGRATE,BIT,DURATION,CHANNELS,INDEX,MAX_FREQ,DB_THRESHOLD,FREQ_STEPS,LEFT_CHANNEL,RIGHT_CHANNEL")
        getindex <- function(soundfile, inCluster = FALSE, ...) {
            if (inCluster == TRUE) {
                require(soundecology)
            }
            args <- list(...)
            if (!is.null(args[["db_threshold"]])) {
                db_threshold = args[["db_threshold"]]
            }
            else {
                db_threshold = formals(acoustic_evenness)$db_threshold
            }
            if (!is.null(args[["max_freq"]])) {
                max_freq = args[["max_freq"]]
            }
            else {
                max_freq = formals(acoustic_evenness)$max_freq
            }
            if (!is.null(args[["freq_step"]])) {
                freq_step = args[["freq_step"]]
            }
            else {
                freq_step = formals(acoustic_evenness)$freq_step
            }
            if (flac == TRUE) {
                soundfile_path <- get_wav(directory, soundfile)
            }
            else {
                if (.Platform$OS.type == "windows") {
                  soundfile_path = paste(directory, "\\", soundfile, 
                    sep = "")
                }
                else {
                  soundfile_path = paste(directory, "/", soundfile, 
                    sep = "")
                }
            }
            if (is.na(from) == FALSE) {
                this_soundfile <- tuneR::readWave(soundfile_path, from = from, 
                  to = to, units = units)
            }
            else {
                this_soundfile <- tuneR::readWave(soundfile_path)
            }
            return_list <- acoustic_evenness(this_soundfile, 
                ...)
            if (this_soundfile@stereo == TRUE) {
                no_channels = 2
            }
            else {
                no_channels = 1
            }
            if (flac == TRUE) {
                file.remove(soundfile_path)
            }
            return(paste("\n", soundfile, ",", this_soundfile@samp.rate, 
                ",", this_soundfile@bit, ",", round(length(this_soundfile@left)/this_soundfile@samp.rate, 
                  2), ",", no_channels, ",", soundindex, ",", 
                max_freq, ",", db_threshold, ",", freq_step, 
                ",", return_list$aei_left, ",", return_list$aei_right, 
                sep = ""))
        }
    }
    time0 <- proc.time()
    cat(fileheader, file = resultfile, append = FALSE)

    if (!is.na(empty[1])) {
        print("Empty files found, removing them from treatment: ")
        print(empty)
       wav_files <- wav_files[!wav_files %in% empty] # Stop empty files from being processed
    }


    if (no_cores > 1) {
        no_files <- length(wav_files)
        if (no_cores > no_files) {
            no_cores <- no_files
            cat("\n The number of cores to use has been reduced because there are less files than cores available\n")
        }
        cat(paste("\n Running the function ", soundindex, "() on ", 
            no_files, " files using ", no_cores, " cores", "\n\n", 
            sep = ""))
        cl <- makeCluster(no_cores, type = "PSOCK")
        res <- parLapply(cl, wav_files, getindex, inCluster = TRUE, 
            ...)
        write.table(res, file = resultfile, append = TRUE, quote = FALSE, 
            col.names = FALSE, row.names = FALSE)
        Sys.sleep(1)
        stopCluster(cl)
    }
    else {
        cat(paste(" Running on ", length(wav_files), " files using 1 core", 
            "\n\n", sep = ""))
        for (soundfile in wav_files) {
            this_res <- getindex(soundfile, ...)
            cat(this_res, file = resultfile, append = TRUE)
        }
    }
    time1 <- proc.time() - time0
    cat(paste(" The analysis of ", length(wav_files), " files took ", 
        round(time1["elapsed"], 2), " seconds\n\n", sep = ""))
}
###########################
## Calculate indices
##########################
## Date initiale: 24 avril 2024
## Author : Noemie Lacroix-D, Mederic Durand

## To do:
# []Logbook
# [x]Loop over each audiomoth folder
# [x]Bind ndsi, h and aci results
# []Comment functions
# []Run this with an audio filter and compare
#       Still need to confirm this, but only 
# []Think about 


#### SETTING WORK DIRECTORY AND SETTINGS ####

source("./6.Soundscape/0.Environment.R")
AudioIndices <- c("ndsi", "H", "acoustic_complexity") # Indices to be computed

####LOAD DATA####

# Parent folder of where the wav are located
pathParent <- "D:/Audiomoths_2024_Summer"

# Parent folder of where the results (csv) should go
pathParentOutput <- "C:/Projects/AudiomothData/TimingAnalysis"

# Filter the sites within the parent folder down to the chosen site (CDQ, CAR, etc)
site <- "MON_CG"
audiomothsPaths <- list.dirs(pathParent, recursive = FALSE)
audiomothsPaths <- audiomothsPaths[grepl(site, audiomothsPaths)]
audiomothsPaths


# This assigns our multiple_indices function as the multiple_sounds function from the soundecology package
unlockBinding("multiple_sounds", as.environment("package:soundecology"))
assign("multiple_sounds", multiple_indices, as.environment("package:soundecology"))
lockBinding("multiple_sounds", as.environment("package:soundecology"))


start <- Sys.time()
for (audiomoth in audiomothsPaths){

       ####GENERATE PATHS####
       codeSite <- sub(".*/","",audiomoth)

       pathAudio <- paste0(pathParent, "/", codeSite,"/")
       pathOutput <- paste0(pathParentOutput, "/", codeSite, "/")

       if (!dir.exists(pathOutput)){
              dir.create(pathOutput, recursive = TRUE)
       }

       ####LOAD DATA####
       DFday <- load_daily(path = pathAudio, site = codeSite)

       ####COMPUTE INDICES####
       i = 1
       for (folder in DFday$foldername){ # Loop over each folder

              subfolder <- DFday$date_raw[i] # 20240415, etc

              # Identify empty .wav files so soundecology package doesn't explode
              # certain empty .wav files are more than 0kb
              # According to File size = Sample rate * bit depth * number of channels * duration (seconds)
              # where sample rate = 48000hz, bit depth = 2 (16 bits), number of channels = 1, then the cut off should be
              # 96,000 bytes = 93.75kb
              # Cut off for 30 seconds or less = 2,880,000

              files_list <- list.files(folder, full.names = TRUE)
              total_files <- length(files_list)
              files_list <- files_list[file.info(files_list)$size < 2880000]

              # Extract filename
              files_list <- basename(files_list)
              
              # Generate empty file list
              empty_files <- c(NA) # Yeah this aint kosher

              if(length(files_list) > 0) {
                     empty_files <- files_list

                     if(length(empty_files) == total_files) {
                            print("No files large enough in this folder. Aborting")
                            break
                     }
              }

              # Then compute each index in parallel
              for (AudioIndex in AudioIndices){

                     print(paste0("Computing ", AudioIndex, " for ", subfolder))

                     multiple_sounds(
                            directory = folder,
                            resultfile = paste0(pathOutput, subfolder ,"_", AudioIndex, ".csv"), # Output in a .csv file
                            soundindex = AudioIndex,
                            no_cores = "max",
                            empty = empty_files)
              }
              i = i+1
       }
}
end <- Sys.time()
end - start

######## Set up ########
# Libraries
suppressPackageStartupMessages({
     library(dplyr)
})

# P-drive path
p_path<-base::file.path("P:","Projets","Actif","2023_ECCC4_Biodiv")

# Species list (either validated or unvalidated species)
file_name<-"Species_already_validated.csv" # "Species_left_to_validate.csv" or "Species_already_validated.csv"
destination<-"D" # "P" or "D"
save_file_location<-base::file.path(p_path,"3-Analyses","1-Data","Biodiversity",file_name) 
species_dataframe<-utils::read.csv(save_file_location,row.names=1)

# Species number to work on
Species_number <- 66
# Completed 66
# Working 66

######## Custom functions ########
copy_species_audio_data<-function(species_name,output_path){
     # Coppies bird species data from P-drive to local folder

     # Arguments
     #species:      Character, species name
     #output_path:  Character, path to the local directory

     # Specify the data folder names/ seasons of sampling
     folder_names<-c(
          "Audiomoths_2023_AudioSamples",
          "Audiomoths_2024_Spring_AudioSamples",
          "Audiomoths_2024_Summer_AudioSamples")
     
     # Check species has downloaded data
     downloaded_data_path<-file.path(output_path,species_name)
     files<-list.files(downloaded_data_path,pattern = ".wav",recursive = TRUE)
     if(length(files)>0){
          return(message("Files already downloaded."))
     }
     
     # Copy the species data from all seasons locally
     message("Copying data ...")
     for(folder in folder_names){
          # Path to data
          data_path<-file.path(p_path,"3-Analyses","1-Data","AudioMoth_recordings",folder)
     
          # List folders inside
          folders_inside<-base::list.files(path=data_path,include.dirs=TRUE)
     
          # Remove Noise, Uncertain, snippet_paths.csv and snippet_paths.xlsx
          folders_inside <- folders_inside[folders_inside != "Noise"]
          folders_inside <- folders_inside[folders_inside != "Uncertain"]
          folders_inside <- folders_inside[folders_inside != "snippet_paths.csv"]
          folders_inside <- folders_inside[folders_inside != "snippet_paths.xlsx"]
     
          # Check if species name is within folder
          species_correct<-paste("Correct",species_name)
          species_incorrect<-paste("INCORRECT",species_name)
          if(species_correct %in% folders_inside){
               # Create path to data 
               species_data_path<-base::file.path(data_path,species_correct)
     
               # Create new output path where data will be coppied to
               output_path_species<-file.path(output_path,species_name,folder)
               if(!base::file.exists(output_path_species)){
                    base::dir.create(output_path_species,recursive=TRUE)
               }

               # Create the destination folder if it doesn't exist
               if (!dir.exists(output_path_species)) {
                    dir.create(output_path_species, recursive = TRUE)
               }
     
               # Copy data
               file.copy(list.files(species_data_path, full.names = TRUE), output_path_species, recursive = TRUE)
          }
          if(species_incorrect %in% folders_inside) {
               # Create path to data 
               species_data_path<-base::file.path(data_path,species_incorrect)
     
               # Create new output path where data will be coppied to
               output_path_species<-file.path(output_path,species_name,folder)
               if(!base::file.exists(output_path_species)){
                    base::dir.create(output_path_species,recursive=TRUE)
               }

               # Create the destination folder if it doesn't exist
               if (!dir.exists(output_path_species)) {
                    dir.create(output_path_species, recursive = TRUE)
               }
     
               # Copy data
               file.copy(list.files(species_data_path, full.names = TRUE), output_path_species, recursive = TRUE)
          }
     }
     message("Done")
}
move_labels<-function(data_path,species_metadata_df,label_path){
     # Coppies data from source to destination

     # Arguments
     #data_path:            Character, path to source data
     #species_metadata_df:  DataFrame, containing metadata on the species to process
     #label_path:           Character, path to where the labels will be moved

     # Check if species has labels
     if(is.null(species_metadata_df[,2])){
          return(message("No labels found for species. Create your own."))
     }
     if(is.null(species_metadata_df[,4])){
          return(message("No labels found for species. Create your own."))
     }
     if(is.null(species_metadata_df[,6])){
          return(message("No labels found for species. Create your own."))
     }

     # Check species has been moved
     moved_data_path<-file.path(label_path,species_metadata_df$Species)
     files<-list.files(moved_data_path,pattern = ".wav",recursive = TRUE)
     if(length(files)>0){
          return(message("Files already moved."))
     }

     # Loop over each season to identify the labels
     seasons<-c("season.x","season.y","season")
     season_folder_names<-c("Audiomoths_2023_AudioSamples","Audiomoths_2024_Spring_AudioSamples","Audiomoths_2024_Summer_AudioSamples")
     statuses<-c("Audio.ID.status.x","Audio.ID.status.y","Audio.ID.status")

     message("Moving files ...")
     for(i in 1:length(seasons)){
          # Grab data
          season_label<-seasons[i]
          status<-statuses[i]
          season<-season_folder_names[i]

          # Grab species name
          species_name<-species_metadata_df$Species

          # Select column names
          subset_df<-species_metadata_df %>%
               dplyr::select(all_of(c(season_label, status)))        
          
          # See if status is "DONE" or "15 SAMPLES", and then move files
          if(is.na(subset_df[,2])){
               next
          }
          if(subset_df[,2] == "DONE"){
               # ID files inside the associated season
               path_season_snippits<-file.path(data_path,species_name,season)

               # List files
               files_inside<-list.files(path_season_snippits,pattern="*.wav")

               # Move files
               for(file in files_inside){
                    source_file<-file.path(path_season_snippits,file)
                    destination_path<-file.path(label_path,species_name)
                    if(!base::file.exists(destination_path)){
                         base::dir.create(destination_path,recursive=TRUE)
                    }
                    destination_file<-file.path(destination_path,file)
                    file.copy(source_file,destination_file)
                    file.remove(source_file)
               }
          }
          if(subset_df[,2] == "15 SAMPLES"){
               # ID files inside the associated season
               path_season_snippits<-file.path(data_path,species_name,season)

               # List files
               files_inside<-list.files(path_season_snippits,pattern="*.wav")

               # Reorder the list of files
               numbers <- as.numeric(sub("\\..*", "", files_inside))
               files_inside <- files_inside[order(numbers)]

               # Select top 15 samples
               files_inside<-files_inside[1:15]

               # Move files
               for(file in files_inside){
                    source_file<-file.path(path_season_snippits,file)
                    destination_path<-file.path(label_path,species_name)
                    if(!base::file.exists(destination_path)){
                         base::dir.create(destination_path,recursive=TRUE)
                    }
                    destination_file<-file.path(destination_path,file)
                    file.copy(source_file,destination_file)
                    file.remove(source_file)
               }
          }
     }
     message("Done")
}

######## Copy data to local relative path ########
# Select species
species_metadata<-species_dataframe[Species_number,]
if(length(species_metadata)==1){
     species<-species_metadata
} else{
     species<-species_metadata$Species
}

# Create local relative directory
output_path<-base::file.path("data","audio")

# Copy data locally
copy_species_audio_data(
     species_name=species,
     output_path=output_path)

######## Move and identify labels ########
# Set up paths
label_path<-base::file.path("data","labels")

# Move files
move_labels(
     data_path=output_path,
     species_metadata_df=species_metadata,
     label_path=label_path)

######## OPEN CONTAINER AND TRAIN CLASSIFIER ########

######## Copy labels to external drive ########
# Select species
species_metadata<-species_dataframe[Species_number,]
if(length(species_metadata)==1){
     species<-species_metadata
} else{
     species<-species_metadata$Species
}

# Path to move files
source_file<-base::file.path("data","labels",species)
if(destination=="P"){
     destination_file<-file.path(p_path,"3-Analyses","1-Data","AudioMoth_recordings","Validation_by_analysts",species)
} else{
     #destination_file<-base::file.path("D:","Bird_labeled_calls",species)
     destination_file<-base::file.path("C:","Users","Jurie","Desktop","Bird_labeled_calls",species)
}
if (!dir.exists(destination_file)) {
     dir.create(destination_file, recursive = TRUE)
}
file.copy(list.files(source_file, full.names = TRUE), destination_file, recursive = TRUE)

# Delete species folders
if(destination=="P"){
     unlink(base::file.path("data","labels"), recursive = TRUE)
     unlink(base::file.path("data","embeddings"), recursive = TRUE)
     unlink(base::file.path("data","example_label"), recursive = TRUE)
     unlink(base::file.path("data","audio"), recursive = TRUE)
} else{
     unlink(base::file.path("data","labels"), recursive = TRUE)
     unlink(base::file.path("data","custom_classifier"), recursive = TRUE)
     unlink(base::file.path("data","embeddings"), recursive = TRUE)
     unlink(base::file.path("data","audio"), recursive = TRUE)
}

######## Updated validation csv meta data ########
# Indicate that you have finished processing a species
species_dataframe$processed[Species_number] <- "Yes"
utils::write.csv(species_dataframe, save_file_location)

########### Libraries ###########
# Install
#install.packages("monitoR")
#install.packages("tuneR")
#install.packages("seewave")
#install.packages(c('dyplr','tidyverse','ggplot2','foreach','doParallel','stringr'))
#install.packages('vegan')
#install.packages('purrr')

# Load
suppressMessages(library(monitoR))
suppressMessages(library(tuneR))
suppressMessages(library(dplyr))
suppressMessages(library(foreach))
suppressMessages(library(doParallel))
suppressMessages(library(stringr))
suppressMessages(library(purrr))
#suppressMessages(library(seewave))

# Path to data
pathData<-"P:/Projets/Actif/2023_ECCC4_Biodiv/3-Analyses/1-Data/"
pathAudio<-"C:/Users/Jurie/Desktop/New_data/"

########### Custom functions ###########
# Define custom function
create_template_samples_core<-function(intput_path,Bird_df_List,output_path,debug=FALSE) {
     ###################################################################################
     #Author
     #Script writin by Kasselman Jurie Theron
     #Copyright 2024 Habitat

     #Description
     #Function prepares directories, clips audio from recordings and saves them to disk
     #to be manually validated

     #Argumemts
     #intput_path        Full path to the input audio recordings folder
     #Bird_df_List       List contatining data frame per recording of oputput from birdnet
     #output_path        Full path to the location of saving audio snippets
     #debug              Boolian, whether to create log file for debugging
     ###################################################################################
     # Create folder for the output for every parallel process
     output_pathh<-output_path 
     if(debug==TRUE){
          output_pathh<-paste0(output_path,as.character(i),"/")
          if (!dir.exists(paste0(output_pathh))) {
               dir.create(paste0(output_pathh))
          }
     }     

     # Debug
     if(debug==TRUE){
          if (!dir.exists(paste0(output_pathh,"Logs/"))) {
               dir.create(paste0(output_pathh,"Logs/"))
          }
          log_file<-paste0(output_pathh,"Logs/log.txt")
          sink(log_file,append=TRUE)
     }

     # Grab dataframe
     Bird_df<-Bird_df_List[[i]]

     # Create path to audio recording
     folder_name<-as.character(unique(Bird_df$folder_name))
     file_name<-as.character(unique(Bird_df$file_name))
     file_name<-stringr::str_replace_all(
          file_name,
          pattern=".BirdNET.selection.table.txt",
          replacement="")
     sub_folder_name<-stringr::str_replace(
          file_name,"_.*","")
     audio_path<-paste0(intput_path,folder_name,"/",
          sub_folder_name,"/",file_name,".WAV")
     
     # Load sample data
     file<-tuneR::readWave(audio_path)

     # Debug
     if(debug==TRUE){
          cat(paste0("File loaded ",as.character(audio_path)))
     }

     # List to store dataframe meta data
     dataframe_list<-list()

     # Prepare dataframe data
     path<-paste0(intput_path,
          folder_name,"/",sub_folder_name,"/",file_name,".WAV")
     parts<-stringr::str_split(path,pattern="/")

     # Loop over dataframe
     for(ii in 1:nrow(Bird_df)){
          # Create recording snippet name
          snippet_name<-as.character(
               paste0(Bird_df[ii,]$New_Sp_name,
                    " ", Bird_df[ii,]$Confidence))

          # Create snippet output folder name
          snippet_folder<-as.character(Bird_df[ii,]$New_Sp_name)

          # Create folder
          if (!dir.exists(paste0(output_pathh,snippet_folder,"/"))) {
               dir.create(paste0(output_pathh,snippet_folder,"/"))
          }

          # Grab index
          index<-Bird_df[ii,]$Index

          # Create snipet output path with extention
          snippter_path<-paste0(
               output_pathh,
               snippet_folder,"/",
               index,".",snippet_name,".wav")
          
          # Extract sample
          Begin<-as.double(Bird_df[ii,]$Begin.Time..s.)
          End<-as.double(Bird_df[ii,]$End.Time..s.)
          Extract_Sample<-tuneR::extractWave(
               file,interact=FALSE,
               from=Begin,to=End,
               xunit="time")

          # Save to disk
          tuneR::writeWave(Extract_Sample,snippter_path)

          # Create dataframe
          max<-length(parts[[1]])
          datafr<-data.frame(
               season=as.character(parts[[1]][max-3]),
               audiomothID=as.character(parts[[1]][max-2]),
               date=as.character(parts[[1]][max-1]),
               wav_name=as.character(parts[[1]][max]),
               clip_name=paste0(index,".",snippet_name,".wav"),
               start=Begin,
               end=End)

          # Add to list
          dataframe_list[[ii]]<-datafr
     }

     # Debug
     if(debug==TRUE){
          cat(paste0("Finished with ",as.character(i)))
     }

     # Clean memory
     rm(file)
     gc()

     # Debug
     if(debug==TRUE){
          sink(NULL)
     }

     # Write metadata
     name<-tools::file_path_sans_ext(parts[[1]][max])
     as_tibble(do.call(base::rbind,dataframe_list)) %>%
          write.csv(paste0(output_pathh,folder_name,as.character(name),"_metaData.csv"))
}
create_template_samples <- function(intput_path,Bird_df_List,output_path,debug=FALSE,cores=10) {
     ###################################################################################
     #Author
     #Script writin by Kasselman Jurie Theron
     #Copyright 2024 Habitat

     #Description
     #Function prepares directories, clips audio from recordings and saves them to disk
     #to be manually validated

     #Argumemts
     #intput_path        Full path to the input audio recordings folder
     #Bird_df_List       List contatining data frame per recording of oputput from birdnet
     #output_path        Full path to the location of saving audio snippets
     #debug              Boolian, whether to create log file for debugging
     ###################################################################################

     # Split dataframe per audio file into list
     # List elements are source audio files
     birdNet_list<-split(Bird_df_List,Bird_df_List$Group)
     #birdNet_list<-birdNet_list[1:2]
     
     # Set up parallel backend
     my_cluster<-parallel::makeCluster(cores,type="PSOCK")
     doParallel::registerDoParallel(cl=my_cluster)

     # Run in parallel
     suppressWarnings({foreach(i=1:length(birdNet_list),.packages=c(
          "monitoR","tuneR","dplyr","stringr"),
          .export="create_template_samples_core") %dopar% {
               create_template_samples_core(
                    intput_path=intput_path,
                    Bird_df_List=birdNet_list,
                    output_path=output_path,
                    debug=debug)
     }})

     # Stop parallel backend
     parallel::stopCluster(cl=my_cluster)
     rm(my_cluster)
     gc()

     # List csv
     csv_files<-list.files(
          path=output_path,
          pattern=".csv",
          full.names=TRUE)

     # Read in files
     csv<-lapply(
          csv_files,
          read.csv)
     
     # Merge
     dataframe<-do.call(
          base::rbind,
          csv)
     
     # Save merged file
     dataframe %>%
          dplyr::arrange(wav_name,clip_name) %>%
          write.csv(paste0(output_path,"snippet_paths.csv"))
     
     # Delete old files
     file.remove(csv_files)     
}
species_index<-function(append_birdnet,old_birdnet_index) {
     ###################################################################################
     #Author
     #Script writin by Kasselman Jurie Theron
     #Copyright 2024 Habitat

     #Description
     #Function to index the number of the species predictions based on new data.
     #This allows more predictions to be added later while keeping index
     #of the old processed data the same.

     #Argumemts
     #append_birdnet        Path to the newly appended birdnet prediction output with new data
     #old_birdnet_index     Path to the olde (before appended) birdnet indexed file
     ###################################################################################
     # Load
     birdnet_appnd<-read.csv(append_birdnet)
     birdnet_index_old<-read.csv(old_birdnet_index)

     # Create unique name to find newly added observation
     birdnet_appnd$Full_file_name<-paste0(birdnet_appnd$root,birdnet_appnd$folder_name,birdnet_appnd$file_name,birdnet_appnd$Begin.Time..s.,birdnet_appnd$Latin.Name,birdnet_appnd$Confidence)
     birdnet_index_old$Full_file_name<-paste0(birdnet_index_old$root,birdnet_index_old$folder_name,birdnet_index_old$file_name,birdnet_index_old$Begin.Time..s.,birdnet_index_old$Latin.Name,birdnet_index_old$Confidence)

     # Identify newly added observation
     birdnet_index_old_full_file_name<-unique(birdnet_index_old$Full_file_name)
     birdnet_appnd_full_file_name<-unique(birdnet_appnd$Full_file_name)
     newly_appended<-base::setdiff(birdnet_appnd_full_file_name,birdnet_index_old_full_file_name)
     filtered_birdnet_appnd<-birdnet_appnd %>%
          dplyr::filter(Full_file_name %in% newly_appended)
     filtered_birdnet_appnd$Full_file_name<-NULL     

     # Find the maximum index for each species
     birdnet_index_old$Full_file_name<-NULL
     max_indices<-birdnet_index_old %>%
          dplyr::group_by(Latin.Name) %>%
          dplyr::summarize(Max_Index = max(Index))
     
     # Remove species not in newly appended data
     max_indices<-max_indices %>%
          dplyr::filter(Latin.Name %in% unique(filtered_birdnet_appnd$Latin.Name))

     # Add index to species
     filtered_birdnet_appnd<-filtered_birdnet_appnd %>%
          dplyr::group_by(Latin.Name) %>%
          dplyr::mutate(Index=row_number()) %>%
          dplyr::ungroup()
     
     # Remove new species
     New_species_indexed<-filtered_birdnet_appnd %>%
          dplyr::filter(!Latin.Name %in% unique(max_indices$Latin.Name))
     Old_species_indexed<-filtered_birdnet_appnd %>%
          dplyr::filter(Latin.Name %in% unique(max_indices$Latin.Name))

     # Add the maximum index to continue the indexing from old indexed data
     Old_species_indexed<-Old_species_indexed %>%
          dplyr::group_by(Latin.Name) 
     Old_species_indexed<-Old_species_indexed %>%
          left_join(max_indices,by="Latin.Name")
     Old_species_indexed$Index<-Old_species_indexed$Index+Old_species_indexed$Max_Index
     Old_species_indexed$Max_Index<-NULL
     Old_species_indexed<-dplyr::ungroup(Old_species_indexed)

     # Merge together
     filtered_birdnet_appnd<-rbind(Old_species_indexed,New_species_indexed)

     # Retuern
     return(filtered_birdnet_appnd)
}

########### Prepare BirdNET output ###########
# Load BirdNET predictions
birdNet<-read.csv(
     paste0(pathAudio,
     "Audiomoths_2024_Summer_Processed/BirdNET_ID_Output.csv"))

# Load nesting birds of Quebec
sp_nesting<-read.csv(
     paste0(pathData,
          "Biodiversity/",
          "Birds_Quebec.csv")) %>%
     dplyr::filter(BTSL=="yes") %>%
     #dplyr::filter(Jan == "Y" | Feb == "Y" | Mar == "Y" | Apr == "Y" | May == "Y" |Jun == "Y" | Jul == "Y" | Aug == "Y" | Sep == "Y" | Oct == "Y" | Nov == "Y" | Dec == "Y") %>%
     #dplyr::filter(Feb=="Y" |Mar=="Y"|Apr=="Y"|May=="Y"|Jun=="Y") %>% # Spring
     dplyr::filter(Apr=="Y"|May=="Y"|Jun=="Y"|Jul=="Y"|Aug=="Y") %>% # Summer
     dplyr::select(c("Latin")) %>%
     dplyr::rename(Latin.Name=Latin) %>%
     dplyr::mutate(QC_Sp_Occ="Correct")

# Perform a join
birdNet<-birdNet %>%
  dplyr::left_join(sp_nesting,by="Latin.Name") %>%
  dplyr::mutate(QC_Sp_Occ=ifelse(is.na(QC_Sp_Occ),"INCORRECT",QC_Sp_Occ))

# Create new name for saving samples
birdNet$New_Sp_name<-paste0(
     birdNet$QC_Sp_Occ," ",
     birdNet$Latin.Name)
rm(sp_nesting)

# Add index to species
birdNet<-birdNet %>%
     group_by(Latin.Name) %>%
     mutate(Index=row_number()) %>%
     ungroup()

# Add unique name
birdNet$Group<-as.character(paste0(birdNet$folder_name,birdNet$file_name))

# Save concatenated birdNet indexed output
write.csv(birdNet,
     file=paste0(pathAudio,
          "/Audiomoths_2024_Spring_Processed/BirdNET_ID_Output_Indexed.csv"))

########### Indexed new data for snippets extraction ###########
# Path to appended BirdNET output
append_Bird_path<-"C:/Users/Jurie/Desktop/New_data/BirdNET_ID_Output.csv"

# Path to outdated Birdnet indexed file
Bird_indexed<-"C:/Users/Jurie/Desktop/New_data/BirdNET_ID_Output_Indexed.csv"

# Add index
New_data_index<-species_index(
     append_birdnet=append_Bird_path,
     old_birdnet_index=Bird_indexed) 

# Load nesting birds of Quebec
sp_nesting<-read.csv(
     paste0(pathData,
          "Biodiversity/",
          "Birds_Quebec.csv")) %>%
     dplyr::filter(BTSL=="yes") %>%
     #dplyr::filter(Jan == "Y" | Feb == "Y" | Mar == "Y" | Apr == "Y" | May == "Y" |Jun == "Y" | Jul == "Y" | Aug == "Y" | Sep == "Y" | Oct == "Y" | Nov == "Y" | Dec == "Y") %>%
     #dplyr::filter(Feb=="Y" |Mar=="Y"|Apr=="Y"|May=="Y"|Jun=="Y") %>% # spring
     dplyr::filter(Apr=="Y"|May=="Y"|Jun=="Y"|Jul=="Y"|Aug=="Y") %>% # summer
     dplyr::select(c("Latin")) %>%
     dplyr::rename(Latin.Name=Latin) %>%
     dplyr::mutate(QC_Sp_Occ="Correct")

# Perform a join
New_data_index<-New_data_index %>%
  dplyr::left_join(sp_nesting,by="Latin.Name") %>%
  dplyr::mutate(QC_Sp_Occ=ifelse(is.na(QC_Sp_Occ),"INCORRECT",QC_Sp_Occ))

# Create new name for saving samples
New_data_index$New_Sp_name<-paste0(
     New_data_index$QC_Sp_Occ," ",
     New_data_index$Latin.Name)
rm(sp_nesting)

# Add unique name
New_data_index$Group<-as.character(paste0(New_data_index$folder_name,New_data_index$file_name))

# Rename for further processing
birdNet<-New_data_index

# Apped to old index file
Bird_indexed<-read.csv(Bird_indexed,row.names=1)
birdNet<-rbind(Bird_indexed,birdNet)
write.csv(birdNet,"C:/Users/Jurie/Desktop/New_data/BirdNET_ID_Output_Indexed.csv")
rm(New_data_index,append_Bird_path,Bird_indexed)

########### Create sound snippets ###########
# Create template directory
temp_dir<-"C:/Users/Jurie/Desktop/New_data/Audiomoths_2024_Summer_AudioSamples/"
#temp_dir<-paste0(pathData,"AudioMoth_recordings/Audiomoths_2023_AudioSamples/")
if (!dir.exists(temp_dir)) {
     dir.create(temp_dir)
}

# Cut dataframe to only show newly index data if needed
#birdNet_newly_indexed<-birdNet %>% 
#  filter(grepl("^C:/",root))

# Create snippets
create_template_samples(
     #intput_path=paste0(pathData,"AudioMoth_recordings/Audiomoths_2023/"),
     #intput_path=paste0(pathAudio,"Audiomoths_2024_Summer/"),
     Bird_df_List=birdNet,
     Bird_df_List=birdNet_newly_indexed,
     output_path=temp_dir,
     debug=FALSE,
     cores=10)

# Merge snippet paths

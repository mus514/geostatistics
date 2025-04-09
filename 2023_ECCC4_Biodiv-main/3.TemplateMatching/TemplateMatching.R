########### Libraries ###########
# Load
suppressMessages(library(monitoR))
suppressMessages(library(tuneR))
suppressMessages(library(dplyr))
suppressMessages(library(foreach))
suppressMessages(library(doParallel))
suppressMessages(library(stringr))
suppressMessages(library(purrr))
suppressMessages(library(seewave))

# Path to data
pathData<-"P:/Projets/Actif/2023_ECCC4_Biodiv/3-Analyses/1-Data/"

########### Custom functions ###########
# Define custom functions
combineTemplates <- function(template_path,cutoff_path,confidence,num=5) {
     ###################################################################################
     #Author
     #Script writin by Mederic Durand and Kasselman Jurie Theron
     #Copyright 2024 Habitat

     #Description
     #Function reads in makeCorTemplate files and combines them to use in corMatch
     #functions within monitoR package

     #Argumemts
     #template_path      Full path to the input audio templates folder
     #cutoff_path        Full path to file containing cutoff values for correalation analysis
     #confidence         Threshold for selecting birdnet predictions (0-100)
     #num                Number of templates per species

     #Returns
     #TemplateList object (monitoR object)
     ###################################################################################

     # Transform confidence  if needed
     if(confidence<1){
          confidence_score<-confidence
     }else{
          confidence_score<-confidence/100
     }

     # List templates
     files <- list.files(path=template_path)
     correct_files <- str_subset(files, pattern="Correct")

     # Loop and process
     results<-list()
     for(cor_file in correct_files){
          # Split
          parts<-unlist(strsplit(cor_file,".",fixed=TRUE))

          # Prepare strings
          index<-parts[1] # Split into parts
          taxa_names<-str_sub(parts[2], 9) # Grab taxa name
          taxa_names<-sub(" 0$","",taxa_names) # Remove 0 from name
          numbers<-paste0("0.",parts[3]) # Grab prediction score

          # Create dataframe with results
          data_frame_results<-data.frame(index=index,score=numbers,species=taxa_names,files=cor_file)

          # Add to list
          results[[cor_file]]<-data_frame_results
     }
     
     # Squash list
     data_frame_results<-do.call(rbind,results)     

     # Filter templates based on score and number of species
     data_frame_results<-data_frame_results %>%
          dplyr::filter(score>=confidence_score) %>%
          dplyr::group_by(species) %>%
          dplyr::slice_max(n=num,order_by=score) %>%
          dplyr::ungroup()

     # Convert back to character stings
     correct_files<-as.character(data_frame_results$files)
     taxa_names<-as.character(paste0(data_frame_results$index,".",data_frame_results$species))

     # Read in filtered templates and set names
     templates<-monitoR::readCorTemplates(dir=template_path,files=correct_files,parallel=FALSE)
     monitoR::templateNames(templates)<-taxa_names

     # Prepare cutoff values for correlation
     cutoff_reference_list <- read.csv(cutoff_path)
     species_cutoff_df <- data.frame(data_frame_results$species)
     colnames(species_cutoff_df) <- "Latin.Name"
     species_cutoff_df <- species_cutoff_df %>%
          left_join(cutoff_reference_list, by="Latin.Name")

     # Adjust cutoff threshold
     # Bug in monitoR that produces error 
     # if only one observation in templatelist
     if(length(monitoR::templateCutoff(templates))==1){
          monitoR::templateCutoff(templates) <- c(default=species_cutoff_df$Template_cutoff)
     } else{
          monitoR::templateCutoff(templates) <- c(species_cutoff_df$Template_cutoff)
     }
     
     # Return
     return(templates)
}
detect_signals <- function(audio_path,templates,detection_out,filter_sr=TRUE) {
     ###################################################################################
     #Author
     #Script writin by Kasselman Jurie Theron
     #Copyright 2024 Habitat

     #Description
     #Function takes templates and correlates them to audio files

     #Argumemts
     #audio_path         Full path to the input audio
     #templates          List of templates for matching
     #detection_out      Full path to where results dataframes are saved
     #filter_sr          Boolean to filter 16000 sampling rate

     #Returns
     #Saves dataframe with results to disk
     ###################################################################################

     # Create output folder
     if (!dir.exists(paste0(detection_out))) {
          dir.create(paste0(detection_out))
     }

     # Grab file name
     parts<-unlist(strsplit(audio_path, "/"))
     Folder<-parts[(length(parts)-2)]
     File<-parts[(length(parts))]
     File<-gsub("\\.WAV$", "",File)
     file_name<-paste0(Folder,"_",File)

     # Remove 16000 sampling rate templates
     if(filter_sr==TRUE){
          i <- 1
          while (i <= length(templates@templates)) {
               if (templates@templates[[i]]@samp.rate==16000) {
                    templates@templates<-templates@templates[-i]
               } else {
                    i <- i + 1
               }
          }
     }

     # Check sampling rate of recordings
     if(filter_sr==TRUE){
          sampling_frq_csv<-read.csv("P:\\Projets\\Actif\\2023_ECCC4_Biodiv\\3-Analyses\\1-Data\\Biodiversity\\AudioMoth_Sampling_Frequ.csv") %>%
               dplyr::filter(Samp_Freq==16000) %>%
               dplyr::select(folder_name)
          if(Folder %in% sampling_frq_csv$folder_name){
               return()
          }else{
               audio_path<-audio_path
          }
     }

     # Calculate correlation scroes
     suppressWarnings(cscores<-monitoR::corMatch(audio_path,templates))

     # Detect peaks
     cdetects<-monitoR::findPeaks(cscores)

     # Collect peaks
     detections<-monitoR::getDetections(cdetects)

     # Save to disk
     write.csv(detections,paste0(detection_out,"Detections_",file_name,".csv"))
}
template_matching <- function(audio_path,template,detection_out,filter_sr=TRUE) {
     ###################################################################################
     #Author
     #Script writin by Kasselman Jurie Theron
     #Copyright 2024 Habitat

     #Description
     #Function to perform template matching in parallel

     #Argumemts
     #audio_path         Full path to the input audio
     #template           Object from combineTemplates() function
     #detection_out      Full path to where results dataframes are saved
     #filter_sr          Boolean to filter 16000 sampling rate

     ###################################################################################
     
     # List paths to audio files
     audio_files<-list.files(
          path=audio_path,
          pattern=".WAV",
          full.names=TRUE,
          recursive=TRUE)

     # Set up parallel backend
     cores<-parallel::detectCores() - 5
     my_cluster<-parallel::makeCluster(cores,type="PSOCK")
     doParallel::registerDoParallel(cl=my_cluster)

     # Run in parallel
     foreach(i=1:length(audio_files),.packages=c(
          "monitoR","tuneR","dplyr","stringr"),
          .export="detect_signals") %dopar% {
               detect_signals(
                    audio_path=audio_files[i],
                    templates=template,
                    detection_out=detection_out,
                    filter_sr=filter_sr)
     }

     # Stop parallel backend
     parallel::stopCluster(cl=my_cluster)
     rm(my_cluster,cores)
     gc()    
}

########### Parallel run ###########
# Combine templates
template<-combineTemplates(
     template_path="C:/Users/Jurie/Desktop/MonitoR_Test/Test3/Templates",
     cutoff_path=paste0(pathData,"Biodiversity/Species_Template_Cutoff.csv"),
     confidence=50,
     num=5)

# Perform template matching
template_matching(
     audio_path="C:/Users/Jurie/Desktop/MonitoR_Test/Test3/Audio/",
     template=template,
     detection_out="C:/Users/Jurie/Desktop/MonitoR_Test/Test3/Output_Detections/",
     filter_sr=FALSE)


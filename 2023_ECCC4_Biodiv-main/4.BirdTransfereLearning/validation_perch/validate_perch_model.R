# Libraries
suppressPackageStartupMessages({
     library(dplyr)
     library(tidyr)
})

# Load Perch custom predictions
perch_predictions_path <- file.path("C:","Projects","2023_ECCC4_Biodiv","data","inference_top_44_species","custom_logit_threshold.csv")
perch_predictions <- read.csv(perch_predictions_path)

# Load Audrey Validation dataset
audrey_validation_path <- file.path("P:","Projets","Actif","2023_ECCC4_Biodiv","3-Analyses","1-Data","AudioMoth_validation","Bird_data_package","Bird_annotations_sheet_merge.csv")
audrey_validation <- read.csv(audrey_validation_path)

# Load name conversion lookup table
name_lookup_path <- file.path("P:","Projets","Actif","2023_ECCC4_Biodiv","3-Analyses","1-Data","AudioMoth_validation","Bird_data_package","Name_to_Latin_lookup.csv")
name_lookup <- read.csv(name_lookup_path)

#wav = unique(perch_predictions$filename)[1] #1 - 10

######### Per 5 s audio window #########
# Loop over and calculate accuracy per 5s recording
for(wav in unique(perch_predictions$filename)){
     # Grab file name
     file_name <- basename(wav)
     wav_name <- tools::file_path_sans_ext(file_name)
     wav_name <- sub("_done$", "", wav_name)
     
     # Filter data to audio file
     perch_predictions_subset <- perch_predictions %>%
          dplyr::filter(filename==wav)
     audrey_validation_subset <- audrey_validation %>%
          dplyr::filter(audio_file_name==wav_name)
     
     # Check if Audrey validated this sample
     if(nrow(audrey_validation_subset)==0){
          next
     }

     # Reshape Audrey data
     audrey_validation_subset <- audrey_validation_subset %>%
          tidyr::pivot_longer(
               cols = starts_with("Species_"),
               names_to = "Species_Type",
               values_to = "label",
               values_drop_na = TRUE
          ) %>%
          dplyr::select(!c("Species_Type","end_time_seconds")) %>%
          dplyr::filter(label!="")
     colnames(audrey_validation_subset)<-c("filename","timestamp_s","label")
     
     # Remove other noise labels
     noise_labels <- c("Animal","Doppler effect plane car","Engine","Helicopter",
          "Leaves rustling","Metal sounds","People talking","Rain","Sirens",
          "Train","Wind howling","Animal; Rainette crucifère","Animal; Squirrel")     
     audrey_validation_subset  <- audrey_validation_subset %>%
          dplyr::filter(!label %in% noise_labels) %>%
          dplyr::rename(Francais=label)

     # Convert Audrey's names to latin names
     name_lookup_subset <- name_lookup %>%
          dplyr::filter(Francais %in% audrey_validation_subset$Francais) %>%
          dplyr::select(Nom.latin,Francais)
     audrey_validation_subset<-dplyr::left_join(audrey_validation_subset,name_lookup_subset,by="Francais")



     # Rename columns of predictions and merge
     colnames(perch_predictions_subset) <- c("filename","timestamp_s","Non.latin","logit")
     perch_predictions_subset$filename <- NULL
     merged_df <- merge(
          perch_predictions_subset,
          audrey_validation_subset, 
          by.x = c("timestamp_s", "Non.latin"), 
          by.y = c("timestamp_s", "Nom.latin"), 
          all = TRUE)

     # Save
     merged_df %>%
          write.csv(
               file.path(
                    "data","inference_top_44_species",
                    paste0(wav_name,"_with_predictions_per_5seconds.csv"))
     )
}

######### Species richness #########
# Loop over and calculate Richness per recording
for(wav in unique(perch_predictions$filename)){
     # Grab file name
     file_name <- basename(wav)
     wav_name <- tools::file_path_sans_ext(file_name)
     wav_name <- sub("_done$", "", wav_name)
     
     # Filter data to audio file
     perch_predictions_subset <- perch_predictions %>%
          dplyr::filter(filename==wav)
     audrey_validation_subset <- audrey_validation %>%
          dplyr::filter(audio_file_name==wav_name)
     
     # Check if Audrey validated this sample
     if(nrow(audrey_validation_subset)==0){
          next
     }

     # Reshape Audrey data
     audrey_validation_subset <- audrey_validation_subset %>%
          tidyr::pivot_longer(
               cols = starts_with("Species_"),
               names_to = "Species_Type",
               values_to = "label",
               values_drop_na = TRUE
          ) %>%
          dplyr::select(!c("Species_Type","end_time_seconds")) %>%
          dplyr::filter(label!="")
     colnames(audrey_validation_subset)<-c("filename","timestamp_s","label")
     
     # Calculate unique species
     perch_unique <- perch_predictions_subset %>%
          dplyr::select(-timestamp_s) %>%
          dplyr::group_by(label) %>%
          dplyr::summarise(average_logit = mean(logit))
     colnames(perch_unique)<-c("Nom.latin","average_logit")
     audrey_unique <- unique(audrey_validation_subset$label)

     # Remove other noise labels
     audrey_unique <- audrey_unique[!audrey_unique %in% c(
               "Animal","Doppler effect plane car","Engine","Helicopter",
               "Leaves rustling","Metal sounds","People talking","Rain",
               "Sirens","Train","Wind howling","Animal; Rainette crucifère","Animal; Squirrel")]
     audrey_unique <- data.frame(Francais=audrey_unique)

     # Convert Audrey's names to latin names
     name_lookup_subset <- name_lookup %>%
          dplyr::filter(Francais %in% audrey_unique$Francais) %>%
          dplyr::select(Nom.latin,Francais)
     audrey_unique<-dplyr::left_join(audrey_unique,name_lookup_subset,by="Francais") %>%
          arrange(Nom.latin)

     # Merge Perch predictions
     validation <- audrey_unique
     validation$predicted <- ifelse(validation$Nom.latin %in% perch_unique$Nom.latin, validation$Nom.latin, NA)
     new_names <- perch_unique %>%
          filter(!Nom.latin %in% validation$Nom.latin)
     new_rows <- data.frame(
       Francais = NA,
       Nom.latin = NA,
       predicted = new_names$Nom.latin
     )
     validation <- rbind(validation, new_rows)

     # Add logits
     perch_unique$predicted<-perch_unique$Nom.latin
     perch_unique$Nom.latin<-NULL
     validation <- dplyr::left_join(validation,perch_unique,by="predicted")

     # Add file name
     validation$wave_file_name<-file_name

     # Rename columns
     colnames(validation)<-c("Audrey_nom_francais","Audrey_non_latin","Perch_nom_latin","Perch_ave_logits","wave_file_name")

     # Save
     validation %>%
          write.csv(
               file.path(
                    "data","inference_top_44_species","unique_species",
                    paste0(wav_name,"_with_predictions.csv"))
     )

}

# Consolidate results
merged_results <- list.files(
     file.path("data","inference_top_44_species","unique_species"),
     pattern = "_with_predictions.csv",
     full.names = TRUE) %>%
     lapply(read.csv)
merged_results <- do.call(rbind,merged_results)

# Save
merged_results %>%
     write.csv(
          file.path(
               "data","inference_top_44_species","unique_species","Merged_results.csv"
          )
     )

# Identify species perch was not trained on
perch_classes <- read.csv("data/custom_classifier/class_list.csv") %>% 
     dplyr::pull(custom)
Audrey_classes <- merged_results %>%
     dplyr::select(Audrey_non_latin) %>%
     dplyr::distinct() %>%
     tidyr::drop_na() %>%
     dplyr::pull(Audrey_non_latin)

setdiff(perch_classes,Audrey_classes)
setdiff(Audrey_classes,perch_classes)
intersect(perch_classes,Audrey_classes)

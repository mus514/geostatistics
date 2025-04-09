########### Libraries ###########
# Load
suppressMessages(library(dplyr))
suppressMessages(library(stringr))
suppressMessages(library(tidyr))
suppressMessages(library(foreach))
suppressMessages(library(doParallel))

########### Custom functions ###########
# Define custom function
single_freq_table<-function(intput_path) {
     ###################################################################################
     #Author
     #Script writin by Kasselman Jurie Theron
     #Copyright 2024 Habitat

     #Description
     #Loads and counts the number of observations per species within a specific folder

     #Argumemts
     #intput_path        Full path to the input audio recordings folder
     ###################################################################################
     # List all files
     files<-list.files(
          intput_path,
          pattern=".wav",
          all.files=TRUE,
          full.names=TRUE,
          recursive=TRUE)
     
     # ID number of species
     parts<-stringr::str_split(files,pattern="/")
     species<-lapply(parts,function(x) x[[3]])
     species<-sub("^Correct ","",species)
     species<-sub("^INCORRECT ","",species)
     unique_species<-unique(species) %>%
          sort()
     
     # ID season
     season<-lapply(parts,function(x) x[[2]]) %>%
          unique()
     
     # Create dataframe with species data in
     df<-data.frame(species=unique_species,Season=season[[1]])

     # Number of observations per species
     obs_species<-table(species) %>%
          as.data.frame()

     # Merge data frames
     df<-left_join(df,obs_species,by="species")

     # Return
     return(df)
}
combine_freq_tables<-function(df_list){
     ###################################################################################
     #Author
     #Script writin by Kasselman Jurie Theron
     #Copyright 2024 Habitat

     #Description
     #Loads and counts the number of observations per species within a specific folder

     #Argumemts
     #df_list        List of dataframes to process
     ###################################################################################
     # rbind list
     df<-do.call(
          base::rbind,
          df_list)
     
     # Pivot longer
     df_wide<-df %>%
          tidyr::pivot_wider(
               names_from=Season,
               values_from=Freq)
     
     # Reclass NA values to 0
     df_wide[is.na(df_wide)]<-0

     # Return
     return(df_wide)
}
create_freq_table<-function(intput_paths,cores="Auto") {
     ###################################################################################
     #Author
     #Script writin by Kasselman Jurie Theron
     #Copyright 2024 Habitat

     #Description
     #Loads and counts the number of observations per species using parallel computing

     #Argumemts
     #intput_paths       List of full path to the input audio recordings folders
     #cores              Number of cores for parallel run
     ###################################################################################

     # ID number of cores to run
     if(is.character(cores)){
          # Set cores to number of folders in intput_paths
          cores<-as.double(length(intput_paths))
     } else if(is.double(cores)){
          # Keep number of cores specified
          cores<-cores
     }
     
     # Loop over folders
     if (!is.null(cores)){
          # Set up parallel backend
          if(is.double(cores)){
               my_cluster<-parallel::makeCluster(cores,type="PSOCK")
               doParallel::registerDoParallel(cl=my_cluster)
          }

          # Run in parallel
          suppressWarnings({csv<-foreach(i=1:length(intput_paths),
               .packages=c("dplyr","stringr"),
               .export="single_freq_table") %dopar% {
                    csv_files<-single_freq_table(
                         intput_path=intput_paths[[i]])
          }})

          # Stop parallel backend
          parallel::stopCluster(cl=my_cluster)
          rm(my_cluster)
          gc()

          # Combine dataframes
          df<-combine_freq_tables(df_list=csv)

          # Return
          return(df)
          
     } else{
          # Run un series
          suppressWarnings({csv<-foreach(i=1:length(intput_paths),
               .packages=c("dplyr","stringr"),
               .export="single_freq_table") %do% {
                    csv_files<-single_freq_table(
                         intput_path=intput_paths[[i]])
          }})

          # Combine dataframes
          df<-combine_freq_tables(df_list=csv)

          # Return
          return(df)
     }          
}
########### Workflow ###########
# Specify paths
pathAudio<-c(
     file.path("D:","Audiomoths_2023_AudioSamples"),
     file.path("D:","Audiomoths_2024_Spring_AudioSamples"),
     file.path("D:","Audiomoths_2024_Summer_AudioSamples_NOT_COMPLETE")
)
save_dir<-file.path(
     "P:","Projets","Actif","2023_ECCC4_Biodiv",
     "3-Analyses","1-Data","AudioMoth_recordings")

# Create frequency tabel
df<-create_freq_table(
     intput_paths=pathAudio,
     cores="Auto")
glimpse(df)

# Sum total frequency
df$Total_obs<-base::rowSums(df[,2:4])
glimpse(df)

# Arrange
df_arrange<-df %>%
     dplyr::arrange(
          desc(Total_obs))

# Save merged file
df_arrange %>%
     write.csv(
          file.path(save_dir,"Species_Frequency_Table.csv"))

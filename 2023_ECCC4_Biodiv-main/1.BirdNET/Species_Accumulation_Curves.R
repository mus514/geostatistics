########### Libraries ###########
# Load
suppressMessages(library(BiodiversityR))
suppressMessages(library(tuneR))
suppressMessages(library(ggplot2))
suppressMessages(library(dplyr))
suppressMessages(library(ggsci))
suppressMessages(library(stringr))
suppressMessages(library(purrr))
suppressMessages(library(foreach))
suppressMessages(library(doParallel))

# Path to data
pathData<-"P:/Projets/Actif/2023_ECCC4_Biodiv/3-Analyses/1-Data/"

########### Custom functions ###########
CalculateAudiomothLength<-function(Audiomoth_Data_path) {
     ###################################################################################
     #Author
     #Script writin by Kasselman Jurie Theron
     #Copyright 2024 Habitat

     #Description
     #Takes path to audiomoth recordings and calculates the length of the file

     #Argumemts
     #Audiomoth_Data_path          Path to the audiomoth file that should be processed

     #Returns
     #Dataframe with nrows of the number of min in the recording
     ###################################################################################
     # Correct path name
     Audiomoth_Data_path<-gsub("\\\\\\\\", "/", Audiomoth_Data_path)
     Audiomoth_Data_path<-gsub("\\\\", "/", Audiomoth_Data_path)
     Audiomoth_Data_path<-gsub("Audiomoths2023_Processed", "Audiomoths2023", Audiomoth_Data_path)

     # ID the length of the recording
     audio<-tuneR::readWave(Audiomoth_Data_path,header=TRUE)
     rec_length<-round(audio$samples/audio$sample.rate,2)

     # Create index
     time_index<-data.frame(
          start=seq(0,(rec_length-60),by=60),
          end=seq(60,rec_length,by=60))
     time_index$start<-time_index$start+1
     time_index[1,1]<-0     

     # Return
     return(time_index)
}
ProcessBirdNet<-function(BirdNet_df,Folder_name) {
     ###################################################################################
     #Author
     #Script writin by Kasselman Jurie Theron
     #Copyright 2024 Habitat

     #Description
     #Takes birdnet prediction dataframe and calculates site by species matrix
     #Columns are species, and rows are sites. Here sites represent 
     #1 min recording intercals.

     #Argumemts
     #BirdNet_df                     The aggregated birdnet predictions dataframe
     #Folder_name                    The folder name to process

     #Returns
     #Bird community occurance dataframe per site
     ###################################################################################
     # Filter to folder
     files<-BirdNet_df %>%
          dplyr::filter(
               folder_name==Folder_name)

     # Grab number of unique files per folder
     file_IDs<-files %>%
          dplyr::select(file_name) %>%
          unique()

     # ID species richness
     unique_sp<-files %>%
          dplyr::select(Latin.Name) %>%
          unique()
     
     # Create dataframe and list for results
     results_df<-data.frame(matrix(NA,nrow=1,ncol=nrow(unique_sp)))
     colnames(results_df)<-unique_sp$Latin.Name
     results_list<-list()

     # Loop over every file, calculate richness per 1min increments
     for(i in 1:nrow(file_IDs)){
          # Filter to single file
          file<-files %>%
               dplyr::filter(
                    file_name==file_IDs[i,])

          # Prepare file path to calculate lengh
          root<-unique(file$root)
          file_name<-unique(file$file_name)
          file_name<-sub("\\.BirdNET\\.selection\\.table\\.txt$",".WAV",file_name)
          file_path<-paste0(root,"/",file_name)

          # Create occurance dataframe where every column is a species
          # and every row is a 1min increment (representing a site)
          # Create time index based on lengh of recording file        
          time_index<-CalculateAudiomothLength(file_path)

          # Loop over every time index
          # See if the index occures within 
          # the file start and end time
          # if yes, add an observation for that species
          for(ii in 1:nrow(time_index)){
               # Grab index
               t_index<-time_index[ii,]

               # Grab species predictions
               sp_predictions<-file %>%
                    dplyr::filter(Begin.Time..s.>t_index$start) %>%
                    dplyr::filter(End.Time..s.<t_index$end)
               
               # Add "site" with 0 species occurances
               if(nrow(sp_predictions)==0){                         
                    results<-results_df
                    rownames(results)<-ii
                    results[]<-0
                    results_list[[paste0(i,"_",ii)]]<-results
               } else{
                    # Count number of rows
                    rows<-nrow(sp_predictions)

                    # Remove dublicate species
                    sp_predictions<-sp_predictions %>%
                         dplyr::group_by(Latin.Name) %>%
                         dplyr::filter(Begin.Time..s.== min(Begin.Time..s.)) %>%
                         as.data.frame()
                    
                    # Select time
                    start<-sp_predictions[1,]$Begin.Time..s.
                    end<-sp_predictions[1,]$End.Time..s.

                    # See if index time falls within the bird prediction
                    if(start>=t_index$start & start<=t_index$end | end>=t_index$start & end<=t_index$end){
                         # Add species occurances
                         results<-results_df
                         rownames(results)<-ii
                         results[]<-0

                         results_sp<-sp_predictions %>%
                              dplyr::select(Latin.Name) %>%
                              dplyr::mutate(Occ=1)
                      
                         results_sp<-tidyr::pivot_wider(
                              results_sp,
                              names_from=Latin.Name,
                              values_from=Occ) %>%
                              as.data.frame()
                         rownames(results_sp)<-ii

                         results<-merge(results,results_sp,all=TRUE)
                         results<-results[-1, ]
                         results[is.na(results)]<-0
                         rownames(results)<-ii

                         results_list[[paste0(i,"_",ii)]]<-results
                    }
                    # Remove data
                    file<-file %>%
                         slice(-(1:rows))
               }
          }
     }
     # Return
     combined<-do.call(dplyr::bind_rows,results_list)
     return(combined)
}
BatchProcessBirdNet<-function(BirdNet_df,Folder_names) {
     ###################################################################################
     #Author
     #Script writin by Kasselman Jurie Theron
     #Copyright 2024 Habitat

     #Description
     #Takes birdnet prediction dataframe and calculates site by species matrix
     #Columns are species, and rows are sites. Here sites represent 
     #1 min recording intercals.

     #Argumemts
     #BirdNet_df                     The aggregated birdnet predictions dataframe
     #Folder_names                   The folder names to process in a list

     #Returns
     #Bird community occurance dataframe per site in a list
     ###################################################################################
     suppressWarnings({
          # Set up parallel backend
          cores<-parallel::detectCores() - 5
          my_cluster<-parallel::makeCluster(cores,type="PSOCK")
          #clusterExport(cl, 'myMean')
          doParallel::registerDoParallel(cl=my_cluster)

          # Process Birdnet data
          # Run in parallel
          Community_Data_list<-foreach(
               i=1:length(Folder_names),
               .packages=c("dplyr","tidyr"),
               .export=c("ProcessBirdNet","CalculateAudiomothLength")
               ) %dopar% {
                    ProcessBirdNet(
                         BirdNet_df,
                         Folder_names[i])
          }

          # Stop parallel backend
          parallel::stopCluster(cl=my_cluster)
          rm(my_cluster,cores)
          gc()

          # Return list
          return(Community_Data_list)})
}
PlotAccumulationCurves<-function(Community_Data_list,Folder_names) {
     ###################################################################################
     #Author
     #Script writin by Kasselman Jurie Theron
     #Copyright 2024 Habitat

     #Description
     #Plots species accumulation curves per site on one plot

     #Argumemts
     #Community_Data_list          List of community dataframes represnting different sites
     #Folder_names                   The folder names to process in a list

     #Returns
     #Plot with accumulation curves
     ###################################################################################
     # Calculate max nrow
     site_max<-max(sapply(Community_Data_list, function(df) nrow(df)))

     # Calculate max sp richness
     sp_max<-max(sapply(Community_Data_list, function(df) ncol(df)))

     # Plot curve per site
     for(i in 1:length(Community_Data_list)){
          # Grab dataframe
          df<-Community_Data_list[[i]]

          # Grab name
          label<-Folder_names[i]

          # Calculate accumulation results for plotting
          Accum<-df %>%
               BiodiversityR::accumresult(
                    method='exact',#rarefaction
                    conditioned=TRUE)

          # Return plot
          if(i == 1){
               BiodiversityR::accumplot(
                    Accum,
                    labels=label,#Site label
                    xlab="time",
                    type="l",
                    col=i,# index number for colour
                    ci=0.3,# Confidance intervals
                    cex=1.5,
                    xlim=c(1,(site_max+1000)), # max nrows(Sites)
                    ylim=c(1,sp_max), # max species richness
                    cex.lab=1,
                    cex.axis=1)
          } else{
               BiodiversityR::accumplot(
                    Accum,
                    addit=TRUE,#Add to existing plot
                    labels=label,#Site label
                    xlab="time",
                    type="l",
                    col=i,# index number for colour
                    ci=0.3,# Confidance intervals
                    cex=1.5,
                    xlim=c(1,(site_max+1000)), # max nrows(Sites)
                    ylim=c(1,sp_max), # max species richness
                    cex.lab=1,
                    cex.axis=1)
          }
     }
}

########### Species accumulation curves ###########
# Load birdnet predictions
BirdNet_df<-read.csv(
     paste0(
          pathData,
          "Audiomoths2023_Processed/BirdNET_ID_Output.csv"))

# Grab folder name for looping
Folder_names<-unique(BirdNet_df$folder_name)

# Prepare birdnet community data in parallel
Community_Data_list<-BatchProcessBirdNet(BirdNet_df,Folder_names)

# Plot curves
PlotAccumulationCurves(Community_Data_list,Folder_names)

# Calculate sampling effort
Sampling_Effort<-sapply(Community_Data_list, function(df) nrow(df))

# Calculate species richness
Species_Richness<-sapply(Community_Data_list, function(df) ncol(df))

# Save
df<-data.frame(site=Folder_names,Sampling_Effort=Sampling_Effort,Species_Richness=Species_Richness)
write.csv(df,"P:\\Projets\\Actif\\2023_ECCC4_Biodiv\\3-Analyses\\2-Analyses\\Species_Curves_2023\\df.csv")

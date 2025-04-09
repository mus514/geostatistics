# Open csv
data_paths<-file.path("C:","Users","Jurie","Desktop","Book1.csv")
csv<-read.csv(data_paths)

# Specify path to data
p_drive<-file.path(
     "P:","Projets","Actif","2023_ECCC4_Biodiv","3-Analyses","1-Data",
     "AudioMoth_recordings","Audiomoths_2024_Spring")

# Copy path
copy_to<-file.path("C:","Users","Jurie","Desktop","Bird_data_package","wav_files")

# Loop over rows in csv
for(i in 1:nrow(csv)){    
     # Create path to file
     path_wav<-file.path(p_drive,csv$folder[i],paste0(csv$file[i],".WAV"))

     # Copy locally
     file.copy(path_wav, copy_to)
}

library(fs)
library(stringr)

select_and_copy_wav_files <- function(input_path, num_samples, min_samples, destination) {
  # Ensure destination directory exists
  dir_create(destination)
  
  # Get all species folders
  species_folders <- dir_ls(input_path, type = "directory")
  
  for (species_folder in species_folders) {
    species_name <- path_file(species_folder)
    destination_species_folder <- path(destination, species_name)
    
    # Skip if species is already copied
    if (dir_exists(destination_species_folder)) {
      next
    }
    
    # Get all .wav files
    wav_files <- dir_ls(species_folder, glob = "*.wav")
    
    # Skip if there are fewer min_samples
    if (length(wav_files) < min_samples) {
      next
    }

    # Extract file names without extensions
    file_names <- path_ext_remove(path_file(wav_files))
    
    # Split names by spaces and extract the last element as the score
    scores <- as.numeric(sapply(strsplit(file_names, " "), tail, 1))
    
    # Order by score descending and select top num_samples (or max available)
    sorted_indices <- order(scores, decreasing = TRUE)
    selected_files <- wav_files[sorted_indices][seq_len(min(num_samples, length(sorted_indices)))]
    
    # Create species folder in destination
    dir_create(destination_species_folder)
    
    # Copy selected files
    file_copy(selected_files, destination_species_folder)
  }
}

# Specify parameters
input_path <- file.path("P:","Projets","Actif","2023_ECCC4_Biodiv","3-Analyses","1-Data","Bird_labeled_calls")
num_samples <- 500
min_samples <- 350
destination <- file.path("C:","Projects","2023_ECCC4_Biodiv","data","labels_top_large")

# Select top samples
select_and_copy_wav_files(input_path, num_samples, min_samples, destination)











select_random_wav_files <- function(input_path, num_samples, min_samples, destination) {
  # Ensure destination directory exists
  dir_create(destination)
  
  # Get all species folders
  species_folders <- dir_ls(input_path, type = "directory")
  
  for (species_folder in species_folders) {
    species_name <- path_file(species_folder)
    destination_species_folder <- path(destination, species_name)
    
    # Skip if species is already copied
    if (dir_exists(destination_species_folder)) {
      next
    }
    
    # Get all .wav files
    wav_files <- dir_ls(species_folder, glob = "*.wav")
    
    # Skip if there are fewer min_samples
    if (length(wav_files) < min_samples) {
      next
    }
    
    # Randomly select num_samples (or max available)
    selected_files <- sample(wav_files, min(num_samples, length(wav_files)))
    
    # Create species folder in destination
    dir_create(destination_species_folder)
    
    # Copy selected files
    file_copy(selected_files, destination_species_folder)
  }
}

#LARGE_TEST_MODEL_52_SPECIES
# Specify parameters
#input_path <- file.path("P:","Projets","Actif","2023_ECCC4_Biodiv","3-Analyses","1-Data","Bird_labeled_calls")
#num_samples <- 300
#min_samples <- 150
#destination <- file.path("C:","Projects","2023_ECCC4_Biodiv","data","labels")

# Select top samples
#select_random_wav_files(input_path, num_samples, min_samples, destination)

# LARGER_TEST_MODEL_57_SPECIES
# Specify parameters
input_path <- file.path("P:","Projets","Actif","2023_ECCC4_Biodiv","3-Analyses","1-Data","Bird_labeled_calls")
num_samples <- 100
min_samples <- 50
destination <- file.path("C:","Projects","2023_ECCC4_Biodiv","data","labels")

# Select top samples
select_random_wav_files(input_path, num_samples, min_samples, destination)







# Set the directory path
path <- file.path("C:","Projects","2023_ECCC4_Biodiv","data","labels")

# List the folders
folders <- list.dirs(path, full.names = FALSE, recursive = FALSE)

# Initialize vectors to store species names and file counts
species <- c()
occ <- c()

# Iterate through each folder and count the number of files
for (folder in folders) {
  species <- c(species, folder)
  file_count <- length(list.files(file.path(path, folder)))
  occ <- c(occ, file_count)
}

# Create the table
bird_table <- data.frame(Species = species, Occurrences = occ)

# Save
write.csv(bird_table,"data/label_data_information.csv",row.names=FALSE)

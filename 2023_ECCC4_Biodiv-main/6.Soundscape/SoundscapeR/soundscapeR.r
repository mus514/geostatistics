
# Test 2 - SoundscapeR ----------------------------------------------------
# Find the files location
filelocs <- ss_find_files(pathAudio)
summary(filelocs)
head(filelocs[[1]])

# Clean the files
filelocs <- ss_assess_files_new(file_locs = filelocs, full_days = T)

# Calculate CVR index
ss_index_calc(file_list = file_locs_clean[[1]],
              output_dir = pathAudio,
              window = 256)


# Create a soundscape
# With my data
island_metadata <- data.frame(plot = c("Station1", "Station2"),
                              first_day = c("2023-10-10", "2023-10-15"),
                              lat = c(-1.58462, -1.64506),
                              lon = c(-59.87211, -59.82035))

island_metadata$folderloc <-  paste0(pathAudio)

fileloc <- island_metadata$folderloc[1]; print(fileloc)
samplerate = 44100
window = 256
index = "CVR"
date = island_metadata$first_day[1]
lat = island_metadata$lat[1]
lon = island_metadata$lon[1]
method = "IsoData"
output = "incidence_freq"
value = NULL

test <- ss_create(fileloc = fileloc, 
                  samplerate = samplerate, 
                  window = window, 
                  index = index, 
                  date = date, 
                  lat = lat, 
                  lon = lon, 
                  method = method, 
                  output = output)


# With example data 
island_metadata <- data.frame(plot = c("Andre", "Mascote_A1", "Mascote_A2", "Mascote_B1", "Mascote_B2"), 
                              first_day = c("2015-10-10", rep("2015-10-16", 4)), 
                              lat = c(-1.58462, -1.64506, -1.6489, -1.64406, -1.65936), 
                              lon = c(-59.87211, -59.82035, -59.83297, -59.84817, -59.83546))

island_metadata$folderloc <- "C:/Users/noemi/Documents/case_study_data"

fileloc <- island_metadata$folderloc[1]

test <- ss_create(fileloc = fileloc, 
                  samplerate = samplerate, 
                  window = window, 
                  index = index, 
                  date = date, 
                  lat = lat, 
                  lon = lon, 
                  method = method, 
                  output = output)

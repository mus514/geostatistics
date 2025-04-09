###########################
## Testing
##########################
## Date : 22 mars 2024
## Author : Noemie Lacroix-D.


#### SETTING WORK DIRECTORY####
source("./6.Soundscape/Environment.R")


## 1. Import data from project
# tempPath <- "P:/Projets/Actif/2023_ECCC4_Biodiv/3-Analyses/1-Data/Audiomoths2023/LAM-2023-007/20231107/"
# filePath <- list.files(tempPath, ".WAV", full.names = T)
# fileNames <- list.files(tempPath, ".WAV", full.names = F)
# for(i in 1:length(filePath)){
#   temp_url <- filePath[[i]]
#   temp_name <- fileNames[[i]]

#   x <- readWave(temp_url)
#   writeWave(x, paste0("~/Habitat/Soundscape/data/Station1_", temp_name))
# }


# Test 1 - Calculer indice avec 1 seul fichier wav
audio_files <- list.files(path = path_audio, pattern = ".WAV", full.names = T)
audio <- readWave(audio_files[1])

# Acoustic complexity index
ACI <- acoustic_complexity(audio)
print(ACI$AciTotAll_left)
summary(ACI)

# Acoustic diversity index (uses Shannon index)
result <- acoustic_diversity(audio)
print(result$adi_left)
summary(result)

# Normalized difference soundscape index
result <- ndsi(audio)
print(result$ndsi_left)
summary(result)

# Bioacoustic index
bioindex <- bioacoustic_index(audio)
print(bioindex$left_area)
summary(bioindex)


## Test 2 - Calculer les indice choisis (ACI, NDSI, H) pour tous les fichiers
# audio d'une journée donnée
# Define path to audio files
audio_path <- "./data/"

# Run not in parallel
start <- Sys.time()
multiple_sounds(
       directory = audio_path,
       resultfile = "ndsi_results.csv",
       soundindex = "ndsi"
)
end <- Sys.time()
end - start # takes 8 min for 1 day worth of recording

# Try to run in parallel if it takes less time
start <- Sys.time()
multiple_sounds(
       directory = audio_path,
       resultfile = "ndsi_results.csv",
       soundindex = "ndsi",
       no_cores = "max"
)
end <- Sys.time()
end - start # takes 1.74 minutes.

# Try to plot that
ndsi_results <- read.csv("ndsi_results.csv")
ndsi_results <- ndsi_results |>
       mutate(time = extract_time(FILENAME))

plot(ndsi_results$LEFT_CHANNEL ~ ndsi_results$time)


## Test 3 - Create spectrogram
# extract signal
snd <- audio@left

# determine duration
dur <- length(snd) / audio@samp.rate
dur # seconds

# determine sample rate
fs <- audio@samp.rate
fs # Hz

# demean to remove DC offset
snd <- snd - mean(snd)

# plot waveform
plot(snd, type = "l", xlab = "Samples", ylab = "Amplitude")

# number of points to use for the fft
nfft <- 1024

# window size (in points)
window <- 256

# overlap (in points)
overlap <- 128

# create spectrogram
spec <- specgram(
       x = snd,
       n = nfft,
       Fs = fs,
       window = window,
       overlap = overlap
)

# discard phase information
P <- abs(spec$S)

# normalize
P <- P / max(P)

# convert to dB
P <- 10 * log10(P)

# config time axis
t <- spec$t

# plot spectrogram
imagep(
       x = t,
       y = spec$f,
       z = t(P),
       col = oce.colorsViridis,
       ylab = "Frequency [Hz]",
       xlab = "Time [s]",
       drawPalette = T,
       decimate = F
)
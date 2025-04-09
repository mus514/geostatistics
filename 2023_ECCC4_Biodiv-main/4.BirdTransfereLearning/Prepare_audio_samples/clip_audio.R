library(tuneR)
library(seewave)

# path to file
#audio_path <- file.path(
#     "P:","Projets","Actif","2023_ECCC4_Biodiv","3-Analyses","1-Data",
#     "AudioMoth_recordings","Audiomoths_2023","LAM-2023-008","20231111",
#     "20231111_150000.WAV")
audio_path <- file.path(
     "C:","Projects","2023_ECCC4_Biodiv","data",
     "example_label","1.People talking.wav")

# Read in file
audio <- tuneR::readWave(audio_path)

# View spectogram
seewave::spectro(audio)

# Cut frequency
audio_clip <- seewave::bwfilter(audio,from=0,to=5000,output="Wave",bandpass=TRUE)

# View spectogram
seewave::spectro(audio_clip)

# Play audio
play(audio_clip)

#2 hour 19 min 02 s
#19 min 22 s

# Specify time in seconds
start_time <- 0
end_time <- 1

# Extract
clipped_audio <- tuneR::extractWave(audio, from = start_time, to = end_time, xunit = "time")

# Save to disk
tuneR::writeWave(audio_clip, "data/example_label/1.People talking_clip.wav")

########### Libraries ###########
# Load
suppressMessages(library(monitoR))
suppressMessages(library(tuneR))
suppressMessages(library(dplyr))
suppressMessages(library(stringr))
suppressMessages(library(purrr))
suppressMessages(library(seewave))

# Path to data
pathData<-"P:/Projets/Actif/2023_ECCC4_Biodiv/3-Analyses/1-Data/AudioSampleTemplates_Final/"

########### Croping templates ###########
# Load audio 
wav_file<-tuneR::readWave(
     paste0(
          pathData,
          "Correct Aix sponsa/",
          "2.Correct Aix sponsa 0.709.wav"))

# Play audio
tuneR::play(wav_file)

# View spectrogram
seewave::spectro(wav_file)

# Cut frequency
wav_file_filter<-seewave::bwfilter(wav_file,from=1500,to=5000,output="Wave",bandpass=TRUE)

# View spectrogram
seewave::spectro(wav_file_filter)

# Cut time
wav_file_filter_cut<-monitoR::cutWave(wav_file_filter,from=1.2,to=1.7)

# View spectrogram
spectro(wav_file_filter_cut)

# Play audio
play(wav_file_filter_cut)

# Save audio to disk
suppressWarnings({tuneR::writeWave(
     wav_file_filter_cut,
     paste0(
          pathData,
          "Template_TextFiles/Wav_templates/",
          "2.Correct Aix sponsa 0.709.wav"))})

# Create template
template<-monitoR::makeCorTemplate(
     paste0(
          pathData,
          "Template_TextFiles/Wav_templates/",
          "2.Correct Aix sponsa 0.709.wav"),
     frq.lim = c(1.5,5),
     name=as.character("2.Correct Aix sponsa 0.709"))

# Write templates to file
monitoR::writeCorTemplates(template,
     dir=paste0(pathData,"Template_TextFiles/"))

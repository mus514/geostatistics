""" Script to train the CNN
"""

# OUTLINE
# Load packages
# Set configuration settings
# Functions
# Load data
    # Walk through folders
    # Transform data (resize)
    # Crop data maybe

# Split data
# Data augmentation 
    # Horizontal Translations (from audio cutoff)
    # Randomly increasing/decreasing cell values (amplitude mixing)
# Load layers
    # Transfer learning: VGGish (Check w Jurie)
    # Conv layers
    # Recurent layer (see Obsidian)
    # Dropout
    # Classification
# Other:
    # Add a non event class
    # Add an engine class
# Train
# Test
    # Output confusion matrix

# Using the same conda environment than the Analyze_BirdNet python notebook
# EXCEPT!

# conda install numpy pandas librosa scipy
# SHOULD BE INSTALLED IN THE PROJECT'S CONDA ENV pip install tensorflow-io 

import os
#import random
import sys
import glob
import librosa
import numpy as np
import pandas as pd
import tensorflow as tf
import tensorflow_io as tfio
import matplotlib.pyplot as plt
import audiomentations
import pedalboard
from sklearn.model_selection import train_test_split



# from sklearn.metrics import confusion_matrix

""" Configuration settings
"""
freq_lo = 0
freq_hi = 16000
sample_rate = 48000
length_max = 3 # In seconds
random_seed = 42 # Used to randomize the split between training, testing and validation data.
#epoch
#batch_size
#toggle_early_stopping


#template_path = "P:\\Projets\\Actif\\2023_ECCC4_Biodiv\\3-Analyses\\1-Data\\AudioSampleTemplates_Final\\"
template_path = "C:\\Users\\MedericDurand\\Desktop\\AudioSampleTemplates_Final\\"



""" Define functions
"""

def load_templates(template_dir:str, sampling_rate:int, mels_input:int, plot_spectrograms:bool):
    
     ###################################################################################
     #Author
     # Script written by Médéric Durand
     # Copyright 2024 Habitat

     #Description
     # Function loops through a directory and loads the templates.
     # Assumes the folders are named such as : <Correct Genus species> e.g. the typical birdNet output
     # Extracts species name from file name, and append to labels list for each occurence
     # Looping over each audio file, filenames are appended to filenames list ,
     # .wav files are loaded as tensors using the tf.io audio library, and transformed from waveform to mel spectrograms using the
     # create_mel_spectrogram function, and appended to images list

     #Argumemts
     # intput_path        Full path to the input audio recordings folder
     # sampling_rate      Sampling rate, in Hz
     # mels_input         Integer. Default is 128. Passed on to the create_mel_spectrogram function.
     # plot_spectrograms  Boolean. Wether to plot spectrograms as they are created.

     #Returns
     # images          List containing the individual spectrograms
     # labels          List containing the individual labels 
     # filenames        List containing each unique filenames
     ###################################################################################


    images = []
    labels = []
    filenames = []

    # List all subdirectories that start with 'Correct'
    folders = [f for f in os.listdir(template_dir) if f.startswith('Correct') &
               os.path.isdir(os.path.join(template_dir, f))]
    print(folders)

    # Loop through subdirectories
    for folder in folders:

        taxon = folder[8:]
        
        # Glob over files in subdirectories and select .wav files
        for file in list(glob.glob(template_path+folder+'\\*.wav')):

            filenames.append(file)

            # Add label to list
            labels.append(taxon)
            
            # Load .wav as tensor
            audio_data = tfio.audio.AudioIOTensor(file) #[samples, Nchannel(s)] We get samples=144001 since 3s*48000Hz
            #<AudioIOTensor: shape=[144001      1], dtype=<dtype: 'int16'>, rate=48000>
            audio_slice = audio_data[:] #Why is this necessary?
            #<tf.Tensor: shape=(144001, 1), dtype=int16, numpy=array([[1197],[ -14],[   7],...,[ 337],[-248],[-400]], dtype=int16)>
            audio_tensor = tf.squeeze(audio_slice, axis=[-1])
            tensor = tf.cast(audio_tensor, tf.float32)/sampling_rate

            mel_spectrogram = create_mel_spectrogram(tensor, plot = plot_spectrograms, taxon = taxon, mels_input = mels_input)

            images.append(mel_spectrogram)

        print(taxon + " loaded")


    return images, labels, filenames

def create_mel_spectrogram(tensor, plot:bool, taxon:str, mels_input:int):

     ###################################################################################
     #Author
     # Script written by Médéric Durand
     # Copyright 2024 Habitat

     #Description
     # Function that creates a mel spectrogram for each tensors loaded with the load_templates function.
     # Makes use of the tfio.audio.spectrogram function to first create a spectrogram, and then convert it to
     # a mel spectrogram using fast fourier transformation. 

     #Argumemts
     # tensor        TensorFlow Object. Tensor object created in the load_templates function
     # plot          Boolean. Wether to plot the spectrograms or not.
     # taxon         String. Taxa name used for plotting
     # mels_input    Integer. Default is 128. Used in the fast fourier transformation to spectrograms. Refer to tf.io.audio.melscale for more information.

     #Returns
     # images          List containing the individual spectrograms
     # labels          List containing the individual labels 
     # filenames        List containing each unique filenames
     ###################################################################################

    # Create spectrogram
    # Spectrogram parameters:

    window_length_samples = int(round(sample_rate * 0.025)) # sample rate times the desired window size (in seconds)
    hop_length_samples = int(round(sample_rate * 0.01)) # 0.01 seems like a good starting point
    fft_length = 2 ** int(np.ceil(np.log(window_length_samples) / np.log(2.0)))


    spectrogram = tfio.audio.spectrogram(
    tensor, nfft=fft_length, window = window_length_samples, stride = hop_length_samples
    )
    # Default: tensor, nfft=512, window=512, stride=256
    # nfft = size of fft
    # window = size of window
    # stride = size of hops between windows

    mel_spectrogram = tfio.audio.melscale(
    spectrogram, rate = sample_rate, mels = mels_input, fmin = freq_lo, fmax = freq_hi
    )

    if(plot == True):

        plt.figure()
        plt.title(taxon + " Spectrogram")
        plt.imshow(tf.math.log(spectrogram).numpy())

        plt.figure()
        plt.title(taxon + "\nMel spectrogram")
        plt.imshow(tf.math.log(mel_spectrogram).numpy())
    
    del spectrogram

    return mel_spectrogram

def validate_shape(images_list:list, ignore_error:bool):
    
    # Get initial shape
    expected_shape = images_list[0].shape
    
    # Loops over spectrogram list to validate if some spectrograms are of a different shape
    # There could be a more efficient way to do this

    i = 0
    for spectrogram in images_list[0:]:
        if spectrogram.shape != expected_shape: 
            print("Spectrogram #"+str(i+1)+" is not of expected shape. \n Filename: "+filenames[i])    
            if ignore_error == False: return False
        i=i+1
    
    return True

       


images, labels, filenames = load_templates(template_dir = template_path, sampling_rate = 48000, 
                                           mels_input= 128, plot_spectrograms = False)

validate_shape(images, ignore_error=True)


""" Splitting the data
"""
# Training/testing split
train_val_images, test_images, train_val_labels, test_labels, train_val_filenames, \
    test_filenames = train_test_split(images, labels, filenames, test_size=0.2, random_state=random_seed)

# Training/validation split
train_images, val_images, train_labels, val_labels, train_filenames, \
    val_filenames = train_test_split(train_val_images, train_val_labels, train_val_filenames, test_size=0.20, random_state=random_seed)

print('Train-Validation-Test split done')

# Clean up
del train_val_images, train_val_labels, train_val_filenames
del images, labels, filenames


dataset = tf.data.Dataset.from_tensors(train_images)
# Note that if tensors contains a NumPy array, and eager execution is not enabled, 
# the values will be embedded in the graph as one or more tf.constant operations. 
# For large datasets (> 1 GB), this can waste memory and run into byte limits of graph serialization. 
# If tensors contains one or more large NumPy arrays, 
# consider the alternative described in https://www.tensorflow.org/guide/data#consuming_numpy_arrays




# Resize the data?

# Data augmentation 
    # Horizontal Translations (from audio cutoff)
    # Random gain: Randomly increasing/decreasing cell values (amplitude mixing)
    # Time shifting bof because song and call rythm is nice
    # Time stretching: Speeding up and slowing down without afefcting pitch
    # Pitch scaling: Changing frequency without
    # Noise addition
    # Low/high/pass-band filters
    # Polarity inversion

exec(open('DataAugFunc.py').read()) # Load data augmentation functions

#aug_test = amplitude_gain(train_images[1])
aug_test = channel_shuffle(train_images[1])

plt.figure()
plt.title("augmented")
plt.imshow(dataset[1])

plt.figure()
plt.title("Non-augmented")
plt.imshow(train_images[1])


# Add more data augmentation techniques
# Create an image data generator function
# Takes a random subset of images and their labels. Apply a random augmentation. Shuffle it in with training data



# TEST: Identifying max frequency value for a given file

# from scipy import signal
# from scipy.io import wavfile
# file = "C:\\Users\\MedericDurand\\Desktop\\AudioSampleTemplates_Final\\Correct Corvus corax\\1.Correct Corvus corax 0.8229.wav"

# sample_rate, samples = wavfile.read(file)
# fft_samples = np.abs(np.fft.fft(samples))

# peak_index=np.argmax(fft_samples) # get indices of the largest amplitude
# max_frequency = peak_index / (len(samples)) * sample_rate


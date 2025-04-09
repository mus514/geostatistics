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




def amplitude_gain(mel_spectrogram):

    change_rate = np.random.uniform(low=1.2, high=3)
    print("amplitude gain rate = ", change_rate)
    mel_spectrogram = mel_spectrogram * change_rate
    return mel_spectrogram

# def noise(mel_spectrogram):
#     noise_amp = 0.005*np.random.uniform()*np.amax(mel_spectrogram)
#     #mel_spectrogram = mel_spectrogram.astype('float64') + noise_amp * np.random.normal(size=mel_spectrogram.shape[0])
#     mel_spectrogram = mel_spectrogram + noise_amp * np.random.normal(size=mel_spectrogram.shape[0])
#     return mel_spectrogram

# Spec Augment: Park et al., 2019
def frequency_masking(mel_spectrogram, param: int):

    mel_spectrogram = tfio.audio.freq_mask(mel_spectrogram, param = param)
    return mel_spectrogram

def time_masking(mel_spectrogram, param:int):

    mel_spectrogram = tfio.audio.time_mask(mel_spectrogram, param = param)
    return mel_spectrogram


def channel_shuffle(mel_spectrogram):
    
    mel_spectrogram = audiomentations.SpecChannelShuffle(mel_spectrogram)
    return mel_spectrogram


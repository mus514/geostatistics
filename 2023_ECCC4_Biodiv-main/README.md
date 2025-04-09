# 2023_ECCC4_Biodiv
This repo uses various methods to analyse audio recordings from AudioMoth devices. The main goal is to build biodiversity indicators to assess site restoration. Alongside the audio processing, there are several remote sensing workflows to compliment and attribute biodiversity indicators.

## 1 BirdNET
BirdNET is a deep learning model developed by Conwell University that can classify bird species from audio data. For the ECCC4 project, Habitat obtained a license to use the model for research purpose. We aim to use BirdNET to create labeled audio snippets, that will be used to train a Habitat Bird Classifier, or to be used as templates for template matching. The notebook is the entry point for the analysis. It has 2 ways of running BirdNET. The first and best way would be to run BirdNET in parallel. The code block to run BirdNET in parallel calls the `BirdNET_Parallel.py` and the `BirdNET_Parallel_functions.py` scripts. Users need to edit the paths in the `BirdNET_Parallel.py` before running it. Alternatively, BirdNET can be run on a single code.

## 2 Template creation
We decided to develop an alternative method to generate bird richness data, in the case that the Habitat Bird Classifier fails to perform well. This method known as template matching uses an example audio snippet (1-5s long) and then tries to match this pattern within a AudioMoth recording. To do this we first need to create labelled snippets, or templates. The `Extract_Audio_Snippets.R` is used to take the predictions from BirdNET and extract the audio snippets as `.wav` files and save them to disk. These snippets per species will be validated by an expert. From the validated snippets, the `Create_Templates.R` is used to create the templates that are the input to the template matching algorithm.

## 3 Template matching
Template matching uses the templates and searches for similar patterns within the raw AuidioMoth recordings. The output of the algorithm is very similar to that of BirNET. The `TemplateMatching.R` scrip has been set up to run template matching in parallel. 

## 4 Bird transfer learning
When all the labelled bird call snippets have been validated, this data will be used as training and validation data to fine tune a VGGish model. The notebook is the entry point to the code and is used to train a fine tuned VGGish model on our data, as well as to perform inference using the fine tuned model.

## 5 Frog CNN training

## 6 Soundscape

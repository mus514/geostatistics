# ARCHIVE
This readme discribes the archived code folders

## 1 Sound embeddings VGGish
This folder was used to test out and set up the VGGish google model. The deep learning model was trained on a YouTube dataset and can classify ~1000 different audio classes. The idea is to use the encoder of this model and stack a smaller decoder model on top to perform bird species classification from our audio files. The notebook just explores how to feed audio data to the VGGish encoder and extract the rich feature embeddings.

## 2 Perch
Perch is another google model that was trained on bird call audio only. It performs similarly to BirdNET, but its open for commercial use. The idea of this notebook was to compare with BirdNET and see if we can rather use Perch. The notebook goes through how to set up Perch, load the data, push it through the model, process the results and save the predictions to disk.

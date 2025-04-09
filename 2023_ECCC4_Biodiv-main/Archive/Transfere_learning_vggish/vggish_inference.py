# Copyright 2017 The TensorFlow Authors All Rights Reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
# Original code: https://github.com/tensorflow/models/blob/master/research/audioset/vggish/vggish_train_demo.py
# ==============================================================================
#
#
# ==============================================================================
# This code has been modified by Kasselman Jurie Theron on 26 June 2024 
# as part of the ECCC4 biodiversity monitoring project.
# Modified code copyright @Habitat 2024
# ==============================================================================

# Load packages
from __future__ import print_function
from random import shuffle
import numpy as np
import os
import tensorflow.compat.v1 as tf # type: ignore
import tf_slim as slim # type: ignore
import vggish_input
import vggish_params
import vggish_slim

# Set flag from command line
flags = tf.app.flags
flags.DEFINE_integer(
    'num_units',100,
    'Number of unites in the fully connected layer stacked ontop of vggish')
flags.DEFINE_integer(
    '_NUM_CLASSES',3,
    'Number of classes in the classification problem. Corrisponds'
    'to the number of birds the model is trying to predict for.')
flags.DEFINE_string(
    'data_path',"",
    'Full path to the input data')
flags.DEFINE_string(
    'model_saved_path',"",
    'Full path to where the trained model should be saved')
FLAGS = flags.FLAGS

def _get_batches():
  #Definition
  #Returns a shuffled batch of examples with labels of all audio classes.
  #
  # Loop over files in folder and
  # convert to vggish input format
  files_list=[]
  folder_indices={}
  folder_index=0
  for root,_,files in os.walk(FLAGS.data_path):
    if root not in folder_indices:
      folder_indices[root]=folder_index
      folder_index+=1
    for file in files:
      # Convert to vggish formate
      examples=vggish_input.wavfile_to_examples(os.path.join(root,file))
      # Create label
      label=np.eye(FLAGS._NUM_CLASSES,dtype=int)[folder_indices[root]-1].reshape(1, -1)
      label=np.repeat(label,examples.shape[0],axis=0)
      # Add to list
      files_list.append(examples)
  # Concatenate results
  concatenate_files=np.concatenate(files_list,axis=0)
  # Return
  return (concatenate_files)

def main(_):
  #Definition
  #Main function to perform training
  #
  # Load model, add head, define loss/optimizer and train
  with tf.Graph().as_default(), tf.Session() as sess:
    # Define VGGish.
    embeddings=vggish_slim.define_vggish_slim(training=False)
    # Define a shallow classification model
    with tf.variable_scope('mymodel'):
      # Add a fully connected layer
      fc=slim.fully_connected(tf.nn.relu(embeddings),FLAGS.num_units)
      # Add a classifier layer at the end (classification head), 
      # consisting of parallel logistic classifiers, one per class. 
      # This allows for multi-class tasks.
      logits=slim.fully_connected(fc,FLAGS._NUM_CLASSES,activation_fn=None,scope='logits')
      prediction=tf.sigmoid(logits, name='prediction')
    # Restore the model from the checkpoint
    saver=tf.train.Saver()
    saver.restore(sess,os.path.join(FLAGS.model_saved_path,'final_model'))
    # Get new data for prediction
    new_features=_get_batches()
    # Get the tensor for the input features
    features_input=sess.graph.get_tensor_by_name(vggish_params.INPUT_TENSOR_NAME)
    # Run the prediction
    predictions=sess.run(prediction,feed_dict={features_input:new_features})
    #
    #To Do.
    #Write code to convert predictions into classes
    #Translate classes into species names
    #
    # Print
    print('Predictions:', predictions)

if __name__ == '__main__':
  #Definition
  #Run the main function to perform training
  tf.app.run()

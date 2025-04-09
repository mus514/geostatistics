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
# This code has been modified by Kasselman Jurie Theron on 25 June 2024 
# as part of the ECCC4 biodiversity monitoring project.
# Modified code copyright @Habitat 2024
# ==============================================================================

# Load packages
from __future__ import print_function
from random import shuffle
import numpy as np
import os
import tensorflow.compat.v1 as tf
import tf_slim as slim
import vggish_input
import vggish_params
import vggish_slim

# Set flag from command line
flags = tf.app.flags
flags.DEFINE_integer(
    'num_batches',30,
    'Number of batches of examples to feed into the model. Each batch is of '
    'variable size and contains shuffled examples of each class of audio.')
flags.DEFINE_integer(
    'num_units',100,
    'Number of unites in the fully connected layer stacked ontop of vggish')
flags.DEFINE_boolean(
    'train_vggish',True,
    'If True, allow VGGish parameters to change during training, thus '
    'fine-tuning VGGish. If False, VGGish parameters are fixed, thus using '
    'VGGish as a fixed feature extractor.')
flags.DEFINE_string(
    'checkpoint','P:\\Projets\\Actif\\2023_ECCC4_Biodiv\\3-Analyses\\2-Analyses\\vggish\\vggish_model.ckpt',
    'Path to the VGGish checkpoint file.')
flags.DEFINE_integer(
    '_NUM_CLASSES',3,
    'Number of classes in the classification problem. Corrisponds'
    'to the number of birds the model is trying to predict for.')
flags.DEFINE_string(
    'data_path',"",
    'Full path to the input data')
flags.DEFINE_string(
    'model_saved_path',"",
    'Full path to where the trained model should be saved. Without model name.')
FLAGS = flags.FLAGS

def _get_batches():
  #Definition
  #Returns a shuffled batch of examples with labels of all audio classes.
  #
  # Loop over files in folder and
  # convert to vggish input format
  files_list=[]
  label_list=[]
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
      label_list.append(label)
  # Concatenate results
  concatenate_files=np.concatenate(files_list,axis=0)
  concatenate_labels=np.concatenate(label_list,axis=0)
  # Zip and shuffel
  labeled_files=list(zip(concatenate_files,concatenate_labels))
  shuffle(labeled_files)
  # Separate and return the features and labels.
  features=[example for (example,_) in labeled_files]
  labels=[label for (_,label) in labeled_files]
  # Return
  return (features,labels)

def main(_):
  #Definition
  #Main function to perform training
  #
  # Load model, add head, define loss/optimizer and train
  with tf.Graph().as_default(), tf.Session() as sess:
    # Define VGGish.
    embeddings=vggish_slim.define_vggish_slim(training=FLAGS.train_vggish)
    # Define a shallow classification model
    with tf.variable_scope('mymodel'):
      # Add a fully connected layer
      fc=slim.fully_connected(tf.nn.relu(embeddings),FLAGS.num_units)
      # Add a classifier layer at the end (classification head), 
      # consisting of parallel logistic classifiers, one per class. 
      # This allows for multi-class tasks.
      logits=slim.fully_connected(fc,FLAGS._NUM_CLASSES,activation_fn=None,scope='logits')
      tf.sigmoid(logits,name='prediction')
      # Add training ops.
      with tf.variable_scope('train'):
        global_step=tf.train.create_global_step()
        # Labels are assumed to be fed as a batch multi-hot vectors, with
        # a 1 in the position of each positive class label, and 0 elsewhere.
        labels_input=tf.placeholder(
            tf.float32,shape=(None,FLAGS._NUM_CLASSES),name='labels')
        # Cross-entropy label loss.
        xent=tf.nn.sigmoid_cross_entropy_with_logits(logits=logits,labels=labels_input,name='xent')
        loss=tf.reduce_mean(xent,name='loss_op')
        tf.summary.scalar('loss',loss)
        # We use the same optimizer and hyperparameters as used to train VGGish.
        optimizer=tf.train.AdamOptimizer(
            learning_rate=vggish_params.LEARNING_RATE,
            epsilon=vggish_params.ADAM_EPSILON)
        train_op=optimizer.minimize(loss,global_step=global_step)
    # Initialize all variables in the model, and then load the pre-trained
    # VGGish checkpoint.
    sess.run(tf.global_variables_initializer())
    vggish_slim.load_vggish_slim_checkpoint(sess,FLAGS.checkpoint)
    # Define Saver to save the model
    saver = tf.train.Saver()
    if not os.path.exists(FLAGS.model_saved_path):
      os.makedirs(FLAGS.model_saved_path)
    # The training loop.
    features_input=sess.graph.get_tensor_by_name(vggish_params.INPUT_TENSOR_NAME)
    for _ in range(FLAGS.num_batches):
      (features,labels)=_get_batches()
      [num_steps,loss_value,_]=sess.run(
          [global_step,loss,train_op],
          feed_dict={features_input:features,labels_input:labels})
      print('Step %d: loss %g' % (num_steps, loss_value))
    # Save the final model
    saver.save(sess,os.path.join(FLAGS.model_saved_path,'final_model'))

if __name__ == '__main__':
  #Definition
  #Run the main function to perform training
  tf.app.run()

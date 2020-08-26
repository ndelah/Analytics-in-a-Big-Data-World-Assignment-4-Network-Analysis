# -*- coding: utf-8 -*-
"""
@author: VictorGueorguiev
"""
from lda2vec import utils, model
from lda2vec.nlppipe import Preprocessor
from pandas.io.json import json_normalize
import pandas as pd
import numpy as np
import json
import re
 

# Data directory
# Where to save preprocessed data
DATA_FOLDER = './data'
CLEAN_DATA_DIR = "./data/clean_data"
EMBEDDING_DIR = "./data/embeddings"
# Name of input file. Should be inside of data_dir
INPUT_FILE = "export.csv"
# Should we load pretrained embeddings from file
load_embeds = True

# Read in data file
reddit_df = pd.read_csv(DATA_FOLDER + '/' + INPUT_FILE)

reddit_df['p_test'] = reddit_df.p.str.extract("selftext:(.*),over", re.DOTALL) 

reddit_df.columns




# Initialize a preprocessor
P = Preprocessor(reddit_df, "p_test", max_features=30000, maxlen=10000, min_count=30)

# Run the preprocessing on your dataframe
P.preprocess()

# Load embeddings from file if we choose to do so
if load_embeds:
    # Load embedding matrix from file path - change path to where you saved them
    embedding_matrix = P.load_glove(EMBEDDING_DIR + "/" + "glove.6B.100d.txt")
else:
    embedding_matrix = None

# Save data to data_dir
P.save_data(clean_data_dir, embedding_matrix=embedding_matrix)


# Load data from files
(idx_to_word, word_to_idx, freqs, pivot_ids,
 target_ids, doc_ids, embed_matrix) = utils.load_preprocessed_data(CLEAN_DATA_DIR, load_embed_matrix=load_embeds)

# Number of unique documents
num_docs = doc_ids.max() + 1
# Number of unique words in vocabulary (int)
vocab_size = len(freqs)
# Embed layer dimension size
# If not loading embeds, change 128 to whatever size you want.
embed_size = embed_matrix.shape[1] if load_embeds else 128
# Number of topics to cluster into
num_topics = 20
# Amount of iterations over entire dataset
num_epochs = 200
# Batch size - Increase/decrease depending on memory usage
batch_size = 4096
# Epoch that we want to "switch on" LDA loss
switch_loss_epoch = 0
# Pretrained embeddings value
pretrained_embeddings = embed_matrix if load_embeds else None
# If True, save logdir, otherwise don't
save_graph = True


# Initialize the model
m = model(num_docs,
          vocab_size,
          num_topics,
          embedding_size=embed_size,
          pretrained_embeddings=pretrained_embeddings,
          freqs=freqs,
          batch_size = batch_size,
          save_graph_def=save_graph)

# Train the model
m.train(pivot_ids,
        target_ids,
        doc_ids,
        len(pivot_ids),
        num_epochs,
        idx_to_word=idx_to_word,
        switch_loss_epoch=switch_loss_epoch)

utils.generate_ldavis_data(data_path, m, idx_to_word, freqs, vocab_size)

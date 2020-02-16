#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Feb 15 23:43:56 2020

@author: joemarlo
"""

import pandas as pd
import numpy as np
import string
import matplotlib.pyplot as plt
from plotnine import *


# read in data
app_plates = pd.read_csv('Inputs/applications.csv')

# filter to where status == Y and N
app_plates = app_plates[[i in ['Y','N'] for i in app_plates.status]]

# filter out smaller plates
short_list = app_plates.plate[np.array([len(i) for i in app_plates.plate]) > 3]

# tokenize the data
plate_ngrams = pd.DataFrame(list(map(parse_plate, short_list)))

# read in the list of bad words
bad_words = pd.read_table("Data/bad_words.txt", header = None).rename(columns={0: 'Word'}).Word.unique()

# density plot of the scores
ngram_nchars = [len(i) for i in bad_words]
lengths = pd.DataFrame({'Length': ngram_nchars}) # must be pandas df
ggplot(scores) +\
 aes(x = 'ngram_nchars') +\
 geom_density()
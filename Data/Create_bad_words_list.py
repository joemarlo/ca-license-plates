#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Feb 15 17:09:46 2020
Create list of bad words from these links
https://www.kaggle.com/nicapotato/bad-bad-words
https://raw.githubusercontent.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en

@author: joemarlo
"""

import pandas as pd
import numpy as np

project_path = 'Dropbox/Data/Projects/ca-license-plates/'

# read in the data
bad_words_1 = pd.read_csv("https://raw.githubusercontent.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en", header = None)
bad_words_2 = pd.read_csv(project_path + "/Inputs/bad-words.csv", header = None)

# append the two dataframe, rename the colum to Word, then take the unique values then sort
bad_words = bad_words_1.append(bad_words_2, ignore_index = True).rename(columns={0: 'Word'}).Word.unique()
bad_words = np.sort(bad_words)

# remove unicode character
bad_words = bad_words[bad_words != '\U0001f595']

# write to txt
np.savetxt(project_path + 'Data/bad_words_py.txt', bad_words, fmt='%s')

#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Feb 15 23:43:56 2020

@author: joemarlo
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from plotnine import *
import textdistance as textd
exec(open("Analyses/Helper_functions.py").read())


# =============================================================================
# prep data
# =============================================================================

# read in data
app_plates = pd.read_csv('Inputs/applications.csv')

# add identifier for CA source
ca_plates = app_plates.loc[:, ['plate', 'status']]
ca_plates['source'] = ['CA'] * len(ca_plates)

# filter to where status == Y and N
ca_plates = ca_plates[[i in ['Y','N'] for i in ca_plates.status]]

# generate faux plates
n_faux = sum(ca_plates.status == "N") - sum(ca_plates.status == 'Y')
faux_plates = [generate_plate() for _ in range(n_faux)]
faux_plates = pd.DataFrame(data = {'plate': faux_plates, 'source': ['faux'] * len(faux_plates)})
faux_plates['status'] = ['Y'] * len(faux_plates)
del n_faux

# combine with CA and drop duplicates
all_plates = ca_plates.append(faux_plates).drop_duplicates()

# =============================================================================
# tokenize
# =============================================================================

# tokenize the data
plate_ngrams = pd.DataFrame([parse_plate(x, ngram_nchar = range(2,5)) for x in all_plates.plate])

# frequency table of ngram lengths
print(pd.Series([len(i) for i in app_plates.plate]).value_counts())

# rename the columns
col_names = ["ngram_" + str(i) for i in range(1, len(plate_ngrams.columns)+1)]
plate_ngrams.rename(columns=dict(zip(plate_ngrams.columns[0:], col_names)),
                    inplace = True)
del col_names

# add original plate and source identiers
plate_ngrams = pd.concat([all_plates.reset_index(drop = True),
                          plate_ngrams.reset_index(drop = True)], 
                         axis = 1)

# pivot the table to a longer format
plate_ngrams = pd.melt(plate_ngrams,id_vars = ['plate', 'source', 'status'])

# drop NAs, sort, and rename
plate_ngrams = plate_ngrams.dropna().sort_values(by = ['plate', 'source', 'status', 'variable'])
plate_ngrams = plate_ngrams.rename(columns={'value': 'ngram'}).drop(columns = 'variable')

# print counts of source and status
print(plate_ngrams.source.value_counts())
print(plate_ngrams.status.value_counts())

# =============================================================================
# bad word matching
# =============================================================================

# read in the list of bad words
bad_words = pd.read_table("Data/bad_words.txt", header = None).rename(columns={0: 'Word'}).Word.unique()

# density plot of the bad word lengths
word_nchars = [len(i) for i in bad_words]
lengths = pd.DataFrame({'Length': word_nchars}) # must be pandas df
ggplot(lengths) +\
 aes(x = 'Length') +\
 geom_density()
 
del lengths, word_nchars


# calcualate text edit distance (fuzzy matching)
# beware memory hog
# for x, y in [(x,y) for x in plate_ngrams.ngram.unique() for y in bad_words]:
#     scores.append(textd.levenshtein.normalized_similarity(x, y))

# scores = [None] * len(plate_ngrams.ngram) * len(bad_words)
# i = 0
# for ng in iter(plate_ngrams.ngram):
#     for bw in iter(bad_words):
#         scores[i] = textd.levenshtein.normalized_similarity(ng, bw)
#         i += 1


    

# from fuzzywuzzy import process, fuzz

# df = pd.DataFrame([['cliftonlarsonallen llp minneapolis MN'],
#         ['loeb and troper llp newyork NY'],
#         ["dauby o'connor and zaleski llc carmel IN"],
#         ['wegner cpas llp madison WI']],
#         columns=['org_name'])

# org_list = df['org_name']

# threshold = 40

# def find_match(x):

#   match = process.extract(x, org_list, limit=2, scorer=fuzz.partial_token_sort_ratio)[1]
#   match = match if match[1]>threshold else np.nan
#   return match

# df['match found'] = df['org_name'].apply(find_match)


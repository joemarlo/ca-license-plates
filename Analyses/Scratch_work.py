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
from fuzzywuzzy import process, fuzz
from sklearn import linear_model
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
faux_plates = pd.DataFrame(data = {'plate': faux_plates,
                                   'source': ['faux'] * len(faux_plates),
                                   'status': ['Y'] * len(faux_plates)
                                   })
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
plate_ngrams.rename(columns = dict(zip(plate_ngrams.columns[0:], col_names)),
                    inplace = True)
del col_names

# add original plate and source identifier
plate_ngrams = pd.concat([all_plates.reset_index(drop = True),
                          plate_ngrams.reset_index(drop = True)], 
                         axis = 1)

# pivot the table to a longer format
plate_ngrams = pd.melt(plate_ngrams, id_vars = ['plate', 'source', 'status'])

# drop NAs, sort, and rename
plate_ngrams = plate_ngrams.dropna().sort_values(by = ['plate', 'source', 'status', 'variable'])
plate_ngrams = plate_ngrams.rename(columns={'value': 'ngram'}).drop(columns = 'variable')

# print counts of source and status
print(plate_ngrams.source.value_counts())
print(plate_ngrams.status.value_counts())

# =============================================================================
# bad word matching
# =============================================================================

# read in the list of bad words and remove duplicates
bad_words = pd.read_table("Data/bad_words.txt", header = None).rename(columns={0: 'Word'}).Word.unique()

# density plot of the bad word lengths
word_nchars = [len(i) for i in bad_words]
lengths = pd.DataFrame({'Length': word_nchars}) # must be pandas df
ggplot(lengths) +\
 aes(x = 'Length') +\
 geom_density()
 
del lengths, word_nchars

# how many ngrams are direct matches
direct_matches = [i in bad_words for i in plate_ngrams.ngram]
pd.Series(direct_matches).value_counts()
plate_ngrams.ngram[direct_matches]

# add the identifier to the main dataframe
plate_ngrams['direct_match'] = direct_matches

# cross tab of matches and status
pd.crosstab(plate_ngrams.status, plate_ngrams.direct_match)


# calcualate text edit distance (fuzzy matching)
# takes about 10min for levenshtein distance
# takes ~2hrs for all three
scores_l = [None] * len(plate_ngrams.ngram.unique())
scores_mra = [None] * len(plate_ngrams.ngram.unique())
scores_ed = [None] * len(plate_ngrams.ngram.unique())
i = 0
for ng in plate_ngrams.ngram.unique():
    word_scores_l = []
    word_scores_mra = []
    word_scores_ed = []
    for bw in bad_words:
        word_scores_l.append(textd.levenshtein.normalized_similarity(ng, bw))
        # word_scores_mra.append(textd.mra.normalized_similarity(ng, bw))
        # word_scores_ed.append(textd.editex.normalized_similarity(ng, bw))
    scores_l[i] = max(scores_for_this_word)
    # scores_mra[i] = max(scores_for_this_word)
    # scores_ed[i] = max(scores_for_this_word)
    i += 1


# turn into dataframe with original ngram
top_matches_td = pd.DataFrame(
    data = {
        'match_score_l': scores_td,
        'match_score_mra': scores_td,
        'ngram': plate_ngrams.ngram.unique()
        })

# merge back with plate_ngrams
plate_ngrams = plate_ngrams.merge(top_matches_td, on = 'ngram', how = 'left')

del top_matches_td

# densities of scores
ggplot(plate_ngrams) +\
 aes(x = 'match_score_td', group = 'status', color = 'status') +\
 geom_density() +\
 geom_rug(alpha = 0.3)


# takes 20+ min of 4ghz i5
scores = []
for x in plate_ngrams.ngram.unique():
    scores.append(
        process.extractOne(
            x,
            bad_words,
            scorer = fuzz.token_sort_ratio)[1])

# turn into dataframe with original ngram
top_matches = pd.DataFrame(data = {'match_score': scores,
                                   'ngram': plate_ngrams.ngram.unique()
                                   })
# merge back with plate_ngrams
plate_ngrams = plate_ngrams.merge(top_matches, on = 'ngram', how = 'left')

del top_matches

ngrams_long = pd.melt(plate_ngrams, id_vars = ['plate', 'source', 'status', 'ngram'])

# densities of scores
ggplot(ngrams_long) +\
 aes(x = 'value', group = 'status', color = 'status') +\
 geom_density() +\
 geom_rug(alpha = 0.3) +\
 facet_wrap('variable', nrow = 2, scales = 'free')
 
del ngrams_long
 
# =============================================================================
# plots 
# =============================================================================

ggplot(ngrams_long) +\
 aes(x = 'value', group = 'status', color = 'status') +\
 geom_density() +\
 geom_rug(alpha = 0.3) +\
 facet_wrap('variable', nrow = 2, scales = 'free')




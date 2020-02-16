#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Feb 15 20:05:27 2020

@author: joemarlo
"""

import pandas as pd
import numpy as np
import string

app_plates = pd.read_csv('Inputs/applications.csv')

# filter to where status == Y and N
app_plates = app_plates[[i in ['Y','N'] for i in app_plates.status]]


def generate_plate(plate_nchar = 7):
    # function generates a single random string
    #  of 7 letters and numbers 

    index = np.random.choice([True, False], size = plate_nchar, replace = True)
    plate = np.array([None] * plate_nchar)
    plate[index] = np.random.choice(
        list(string.ascii_uppercase), 
        size = sum(index), 
        replace = True)
    plate[index == 0] = np.random.choice(
        range(0, 10),
        size = sum(index == False),
        replace = True)
    
    # return a single collapsed string
    return "".join([str(char) for char in plate])


def parse_plate(plate, ngram_nchar = 3):
    # function takes a single plate and returns all 
    #  "forward" combinations of the letters
    # e.g. plate "1234" would return "12, 123, 1234, 23, 234, 34"
    # ngram.nchar is the amount of characters the returned ngrams should be
    
    # check if ngram_nchar is too long and is the right type
    if isinstance(ngram_nchar, int):
        if ngram_nchar > len(plate):
            raise ValueError('ngram_nchar should be less than or equal to length of plate')
    elif isinstance(ngram_nchar, range):
        if max(ngram_nchar) > len(plate):
            raise ValueError('ngram_nchar should be less than or equal to length of plate')
    else:
        raise TypeError('ngram_nchar should be type integer or range')
    
    # remove all spaces
    plate = plate.replace(" ", "")
    
    tokens = []
    
    # loop through the plate and take every "forward"
    #  combination of letters
    if len(plate) == 1:
        tokens = plate
    else:
        plate_len = len(plate)
        for bgn in range(0, plate_len):
            for end in range(bgn + 1, plate_len + 1):
                tokens.append(plate[bgn:end])
    
    # filter out ngrams that aren't in ngram_char
    nchar = [len(i) for i in tokens]
    if isinstance(ngram_nchar, int):
        index = [i in [ngram_nchar] for i in nchar]
    elif isinstance(ngram_nchar, range):
        index = [i in ngram_nchar for i in nchar]
    tokens = np.array(tokens)[index]

    # remove duplicates and ensure it is lowercase        
    tokens = np.unique(tokens)
    tokens = [i.lower() for i in tokens] 
    
    return tokens

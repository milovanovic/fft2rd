#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Aug 24 11:39:35 2021

@author: marijap
"""

import numpy as np
import math

def reversebits(n, w):
    result = 0
    for i in np.arange(w):
        result <<= 1
        result |= n & 1
        n >>= 1
    return result

def bitrevorder(x):
    w = math.log2(len(x))
    indxs = [int(reversebits(i, w)) for i in np.arange(len(x))]  
    return [x[i] for i in indxs]
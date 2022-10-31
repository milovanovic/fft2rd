#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Sep 3 11:48:20 2021

@author: marijap
"""

# two dimensional FFT

import numpy as np
from radix2DIF import radix2DIF
import scipy.fft as fft
import math

#try to eliminate append function here
def fft2radix2DIF(radarDataMatrix,
                  rangeGrowLogic, 
                  dopplerGrowLogic,
                 # radix = "2", 
                 # decimType = "DIF", 
                  isFp = False,
                  qformatRangeIn =  {'signed': True, 'm': 2, 'n': 10, 'rounding': 'convergent', 'overflow': 'clamp', 'overflow_alert': 'ignore'},
                  qformatDopplerIn = {'signed': True, 'm': 6, 'n': 10, 'rounding': 'convergent', 'overflow': 'clamp', 'overflow_alert': 'ignore'},
                  qformatRangeTwiddle = {'signed': True, 'm': 2, 'n': 14, 'rounding': 'convergent', 'overflow': 'clamp', 'overflow_alert': 'ignore'},
                  qformatDopplerTwiddle = {'signed': True, 'm': 2, 'n': 14, 'rounding': 'convergent', 'overflow': 'clamp', 'overflow_alert': 'ignore'}
                  ):
        
    columnNum = radarDataMatrix.shape[1]
    rowNum = radarDataMatrix.shape[0]
    nextPow2Column = pow(2, math.ceil(np.log(columnNum)/np.log(2)))    
    nextPow2Row = pow(2, math.ceil(np.log(rowNum)/np.log(2)))
    
   # rangeFFTMatrix = np.empty((0, nextPow2Column), complex)
    rangeFFTMatrix = np.zeros((rowNum, columnNum), dtype = complex)
    #dopplerFFTMatrix = np.empty((nextPow2Row, 0), complex)
    dopplerFFTMatrix = np.zeros((rowNum, columnNum), dtype = complex)
    paddedMatrix = np.zeros((nextPow2Row, nextPow2Column), dtype = complex)
    paddedMatrix[:rowNum, :columnNum] = radarDataMatrix 
    
    # Note: chirps should be stored in memory row wise if we want to compare result with scipy.fft
    for rowIdx in range(nextPow2Row): 
      qformatRangeTmp = qformatRangeIn.copy()
      inToRangeFFT = paddedMatrix[rowIdx, :]
      rangeFFT = radix2DIF(inToRangeFFT, rangeGrowLogic, isFp, qformatRangeTmp, qformatRangeTwiddle)
      rangeFFTMatrix[rowIdx,:] = rangeFFT
#     rangeFFTMatrix np.append(rangeFFTMatrix, np.array([rangeFFT]), axis = 0) 
    
    for columnIdx in range(nextPow2Column): 
      inToDopplerFFT = rangeFFTMatrix[:, columnIdx]
      # if columnIdx == (nextPow2Column-1):
      #   print(inToDopplerFFT)
      qformatDopplerTmp = qformatDopplerIn.copy()
      dopplerFFT = radix2DIF(inToDopplerFFT, dopplerGrowLogic, isFp, qformatDopplerTmp, qformatDopplerTwiddle)
      #dopplerFFTMatrix = np.append(dopplerFFTMatrix, np.array([dopplerFFT]).transpose(), axis = 1)
      dopplerFFTMatrix[:, columnIdx] = np.array([dopplerFFT])#.transpose() 
    
    print(dopplerFFTMatrix.shape)
    return  rangeFFTMatrix, dopplerFFTMatrix    

 
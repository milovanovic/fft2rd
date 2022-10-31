#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Aug 22 16:21:42 2021

@author: marijap
"""
import numpy as np

from fixedpoint import FixedPoint
from complexlib import subComplex, div2Complex, addComplex, mulComplex
from utils import  bitrevorder
import math

def radix2DIF(signal,
              grow, 
              isFp = False,
              qformatIn =  {'signed': True, 'm': 2, 'n': 14, 'rounding': 'convergent', 'overflow': 'clamp', 'overflow_alert': 'ignore'},
              qformatTwiddle = {'signed': True, 'm': 2, 'n': 14, 'rounding': 'convergent', 'overflow': 'clamp', 'overflow_alert': 'ignore'}):
    
    nextPow2 = pow(2, math.ceil(np.log(len(signal))/np.log(2)))    
    # convert input signal to fixed point representation
    x = np.zeros(len(signal), dtype = complex)
    y = np.zeros(len(signal), dtype = complex)   
    
    zeros = np.zeros(nextPow2 - len(signal), dtype = complex)
    testSignal = []
    
    # do zero padding if input signal is not power of two
    if (len(zeros) == 0):
        testSignal = signal
    else:     
        testSignal = signal.extend(zeros)
        
    if isFp:
        x = [complex(float(FixedPoint(i.real, **qformatIn)), float(FixedPoint(i.imag, **qformatIn))) for i in testSignal]
        y = [complex(float(FixedPoint(i.real, **qformatIn)), float(FixedPoint(i.imag, **qformatIn))) for i in testSignal]
    else:
        x = testSignal
        
    N = len(testSignal)
    n = np.arange(N)
    S = int(math.log2(N))  
    qformatInS = qformatIn
    
    for stage in range(S):    
        #DIF
        n = np.arange(int(math.pow(2, (S-stage-1))))
        rptNum = math.pow(2, stage)
        num = 0
        i = 0
        
        # default context for input and output data
        if grow[stage] == 1:
            qformatInS['m'] = qformatInS['m'] + 1
           
        qformatOutTmp = qformatInS.copy()
        qformatOutTmp['m'] = qformatOutTmp['m'] + 1
        
        for p in range(0, N, 2):
            if (num < rptNum):
                W = np.exp(-2j*np.pi/N*rptNum*n[i])
            else:
                i = i + 1
                num = 0
                W = np.exp(-2j*np.pi/N*rptNum*n[i])
            num += 1
            if isFp:
                if stage % 2 == 0:
                    add = addComplex(x[int((p + 1)//2)], x[int((p + 1)//2 + N/2)], qformatInS, qformatInS, qformatOutTmp)
                    subtract = subComplex(x[int((p + 1)//2)], x[int((p + 1)//2 + N/2)], qformatInS, qformatInS, qformatOutTmp)
                    if (grow[stage] == 0):
                        y[p] = div2Complex(add, qformatInS, qformatInS)
                        div2Subtract = div2Complex(subtract, qformatOutTmp, qformatInS)
                        y[p + 1] =  mulComplex(div2Subtract, W, qformatInS, qformatTwiddle, qformatInS) 
                    else:
                        y[p] = add
                        y[p + 1] =  mulComplex(subtract, W, qformatOutTmp, qformatTwiddle, qformatOutTmp)
                else:
                    add = addComplex(y[int((p + 1)//2)], y[int((p + 1)//2+ N/2)], qformatInS, qformatInS, qformatOutTmp)
                    subtract = subComplex(y[int((p + 1)//2)], y[int((p + 1)//2 + N/2)], qformatInS, qformatInS, qformatOutTmp)
                    if (grow[stage] == 0):
                        x[p] = div2Complex(add, qformatInS, qformatInS)
                        div2Subtract = div2Complex(subtract, qformatOutTmp, qformatInS)
                        x[p + 1] =  mulComplex(div2Subtract, W, qformatInS, qformatTwiddle, qformatInS)
                    else:
                        x[p] = add
                        x[p + 1] =  mulComplex(subtract, W, qformatOutTmp, qformatTwiddle, qformatOutTmp)
            else: 
                if stage % 2 == 0:
                    add = x[int((p + 1)//2)] + x[int((p + 1)//2 + N/2)]
                    subtract = x[int((p + 1)//2)] -  x[int((p + 1)//2 + N/2)]
                    y[p] = add
                    y[p + 1] = subtract * W
                else:
                    add = y[int((p + 1)//2)] +  y[int((p + 1)//2 + N/2)]
                    subtract = y[int((p + 1)//2)] -  y[int((p + 1)//2 + N/2)]
                    x[p] = add
                    x[p + 1] = subtract * W
    #print("end_radix2")    
    if S % 2 == 1:
        return bitrevorder(y)                
    else:
        return bitrevorder(x)

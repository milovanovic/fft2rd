#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Aug 24 11:36:25 2021

@author: marijap
"""

from fixedpoint import FixedPoint

def addComplex(in1,
               in2, 
               qformatIn1,
               qformatIn2,
               qformatOut):
    
    # I would like to add additional parameter such as choose betweeen floating and fixed point, and to kwargs for the case when 
    
    in1RealFp = FixedPoint(in1.real, **qformatIn1)
    in2RealFp = FixedPoint(in2.real, **qformatIn2)
    
    in1ImagFp = FixedPoint(in1.imag, **qformatIn1)
    in2ImagFp = FixedPoint(in2.imag, **qformatIn2)
    
    #print(in1RealFp + in2RealFp)
    outReal = FixedPoint(float(in1RealFp + in2RealFp), **qformatOut)
    outImag = FixedPoint(float(in1ImagFp + in2ImagFp), **qformatOut)
    #print(in1ImagFp + in2ImagFp)
    return complex(float(outReal), float(outImag)) 

def subComplex(in1,
               in2,
               qformatIn1,
               qformatIn2,
               qformatOut):
    
    in1RealFp = FixedPoint(in1.real, **qformatIn1)
    in2RealFp = FixedPoint(in2.real, **qformatIn2)
    
    in1ImagFp = FixedPoint(in1.imag, **qformatIn1)
    in2ImagFp = FixedPoint(in2.imag, **qformatIn2)
    
    outReal = FixedPoint(float(in1RealFp - in2RealFp), **qformatOut)
    outImag = FixedPoint(float(in1ImagFp - in2ImagFp), **qformatOut)
    
    return complex(float(outReal), float(outImag)) 


# three or four complex multipliers
def mulComplex(in1, 
               in2,
               qformatIn1,
               qformatIn2,
               qformatOut,
               mulType = 'complexUse4Muls',
               scale = 0):
    
    scaleFactor = pow(2, scale)
    in1RealFp = FixedPoint(in1.real, **qformatIn1)
    in2RealFp = FixedPoint(in2.real, **qformatIn2)    
    
    in1ImagFp = FixedPoint(in1.imag, **qformatIn1)
    in2ImagFp = FixedPoint(in2.imag, **qformatIn2)
    
    if (mulType == 'complexUse4Muls'):
        outReal = FixedPoint(float((in1RealFp * in2RealFp) - (in1ImagFp * in2ImagFp))/scaleFactor, **qformatOut)
        outImag = FixedPoint(float((in1RealFp * in2ImagFp) + (in1ImagFp * in2RealFp))/scaleFactor, **qformatOut)
        return complex(float(outReal), float(outImag))
    else:
        return
        a = in2RealFp + in2ImagFp
        b = in1RealFp + in1ImagFp
        c = in1RealFp - in1ImagFp
        d = in1RealFp * a
        e = b * in2ImagFp
        f = c * in2RealFp
        outReal = FixedPoint(float(d - e)/scaleFactor, **qformatOut)
        outImag = FixedPoint(float(d + f)/scaleFactor, **qformatOut)
        return complex(float(outReal), float(outImag))

def div2Complex(in1,
                qformatIn,
                qformatOut,
                scale = 1):
    
    scaleFactor = pow(2,scale)    
    in1RealFp = FixedPoint(in1.real, **qformatIn)
    in1ImagFp = FixedPoint(in1.imag, **qformatIn)

    outReal = FixedPoint(float(in1RealFp)/scaleFactor, **qformatOut)
    outImag = FixedPoint(float(in1ImagFp)/scaleFactor, **qformatOut)
    
    return complex(float(outReal), float(outImag)) 

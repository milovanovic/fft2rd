#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Dec  3 14:23:20 2021

@author: marijap
"""

def twosComplement_hex(hexval):
    bits = 16 # Number of bits in a hexadecimal number format
    val = int(hexval, bits)
    if val & (1 << (bits-1)):
        val -= 1 << bits
    return val

def read_numbers(filename, base):
    with open(filename) as f:
        return [int(x, base) for x in f]

def read_hex_numbers(filename, base):
    with open(filename) as f:
        return [twosComplement_hex(x) for x in f]
    
def tohex(val, nbits, bytesnum = '04x'):
  if (val >= 0):
    return format(int(val), bytesnum)
  else: 
    return hex((val + (1 << nbits)) % (1 << nbits))[2:]
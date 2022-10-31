#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Oct 26 00:13:11 2022

@author: marijap
"""


import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import scipy.fft as fft
from fft2radix2DIF import fft2radix2DIF
from calc_utils import tohex

plt.close('all')

def twos_complement(hexstr, bits):
#     bits = 16
     value = int(hexstr,16)
     if value & (1 << (bits-1)):
         value -= 1 << bits
     return value 
    
def write_data_to_file(filename, data):
    file_to_write = open(filename, "w")
    for i in range(len(data)):
        file_to_write.write(data[i] + '\n')
    file_to_write.close()

def write_complex_data_hex_to_file(filename, data_real, data_imag):
    file_to_write = open(filename, "w")
    for i in range(len(data_real)):
        file_to_write.write(tohex(int(data_real[i]), 16) + tohex(int(data_imag[i]), 16) + '\n')
    file_to_write.close()

def  seperate_real_and_imag(data):
    total_data = len(data)
    real_data = np.zeros(total_data, dtype = int)
    imag_data =  np.zeros(total_data, dtype = int)
    
    data_count = 0
    for i in range(total_data):
        real_data[data_count] = twos_complement(data[i][0:4], 16)
        imag_data[data_count] = twos_complement(data[i][4:8], 16)
        data_count = data_count + 1
    
    return real_data, imag_data

def repack_raw_data(data_to_repack, radar_conf_info): 
    num_rxs = radar_conf_info['num_rxs']
    doppler_fft_size = radar_conf_info['doppler_fft_size']
    num_txs = radar_conf_info['num_txs']
    range_fft_size = radar_conf_info['range_fft_size']
    total_in_data = len(data_to_repack)
    real_data, imag_data = seperate_real_and_imag(data_to_repack)
            
    in_complex_data_rxo = real_data[0:total_in_data:4] + 1j*imag_data[0:total_in_data:4]
    in_complex_data_rx1 =  real_data[1:total_in_data:4] + 1j*imag_data[1:total_in_data:4]
    in_complex_data_rx2 =  real_data[2:total_in_data:4] + 1j*imag_data[2:total_in_data:4]
    in_complex_data_rx3 = real_data[3:total_in_data:4] + 1j*imag_data[3:total_in_data:4]
    
    radar_data_cube = np.zeros((num_rxs, doppler_fft_size, num_txs, range_fft_size), dtype = complex) 
    data_count = 0

    for d in range(doppler_fft_size):
        for t in range(num_txs):
            for s in range(range_fft_size):
               radar_data_cube[0][d][t][s] = in_complex_data_rxo[data_count]
               radar_data_cube[1][d][t][s] = in_complex_data_rx1[data_count]
               radar_data_cube[2][d][t][s] = in_complex_data_rx2[data_count]
               radar_data_cube[3][d][t][s] = in_complex_data_rx3[data_count]
               data_count = data_count + 1
    return radar_data_cube        


# right now it works only for 776
def delete_header(real_data_with_header, imag_data_with_header, mode):
    data_count = 0
    total_data = len(real_data_with_header)
    header_positions = np.arange(0, total_data, mode+1)  # 0 193 396  # make this parametrizable! # 192 or 256
    total_data_without_header = total_data - len(header_positions)
    
    real_data = np.zeros(total_data_without_header, dtype = int)
    imag_data = np.zeros(total_data_without_header, dtype = int)
    
    for i in range(len(real_data_with_header)):
       if i not in header_positions:
           real_data[data_count] = real_data_with_header[i]                          
           imag_data[data_count] = imag_data_with_header[i]
           data_count = data_count + 1
    
    complex_fft2_data = real_data + 1j*imag_data
    return complex_fft2_data
    
# again 772     
def repack_fft2_data(data_to_repack, radar_conf_info, mode):    
    data_count = 0
    num_rxs = radar_conf_info['num_rxs']
    doppler_fft_size = radar_conf_info['doppler_fft_size']
    num_txs = radar_conf_info['num_txs']
    range_fft_size = radar_conf_info['range_fft_size']
    
    real_data_with_header, imag_data_with_header = seperate_real_and_imag(data_to_repack)
    complex_fft2_data = delete_header(real_data_with_header, imag_data_with_header, mode)
    fft2_data_cube = np.zeros((num_rxs*num_txs, doppler_fft_size, range_fft_size), dtype = complex)
    
    if mode == 192:
        for s in range(range_fft_size):
            for t in range(num_txs):
                for r in range(num_rxs):
                    for d in range(doppler_fft_size):
                        fft2_data_cube[t*num_rxs + r][d][s] = complex_fft2_data[data_count]
                        data_count = data_count + 1
    else:
        for t in range(num_txs):
            for r in range(num_rxs):
                for s in range(range_fft_size):
                    for d in range(doppler_fft_size):
                        fft2_data_cube[t*num_rxs + r][d][s] = complex_fft2_data[data_count]
                        data_count = data_count + 1
                
    return complex_fft2_data, fft2_data_cube
     
# work ok for both 192 and 256   
def insert_header_and_write_data_to_file(data, mode, range_fft_size, filename):
    file_to_write = open(filename, "w")
#    extra_data = int(len(data) / mode)
#    total_data = len(data) + extra_data
    headerFirstTwoBytes = "00a5"
    isHalf = False
    cntRow  = 0
    for i in range(len(data)):
        if i%mode == 0:
            header = headerFirstTwoBytes + tohex(int(isHalf == True), 8, '02x') + tohex(int(cntRow), 8, '02x')
            file_to_write.write(header + '\n')
            file_to_write.write(tohex(int(data[i].real), 16) + tohex(int(data[i].imag), 16) + '\n')
            if isHalf:
                cntRow = cntRow + 1
            isHalf = not (False | isHalf)
            if cntRow == range_fft_size:
                cntRow = 0
        else:
            file_to_write.write(tohex(int(data[i].real), 16) + tohex(int(data[i].imag), 16) + '\n')
    file_to_write.close() 
     
def plot_fft2_linear(fft2_matrix, extent_values, r, t):
    fft2_matrix_abs = np.abs(fft2_matrix)   
    fft2_matrix_abs_shifted = np.fft.fftshift(fft2_matrix_abs, 0)
    
    plt.imshow(fft2_matrix_abs_shifted, extent = extent_values, cmap = 'jet', aspect = 7)
    plt.colorbar()
    plt.xlabel('Range [m]')
    plt.ylabel('Velocity [m/s]')
    plt.title('Doppler-Range Heatmap ' + 'rx = %d' % r + ' and ' + 'tx = %d' % t)  
    plt.show()     
        
def plot_fft2_log(fft2_matrix, extent_values, r, t):
    fft2_matrix_abs = np.abs(fft2_matrix)   
    fft2_matrix_log = np.log(fft2_matrix_abs)
    fft2_matrix_log_postproc = np.where(fft2_matrix_log != np.NINF, fft2_matrix_log, 0)
    fft2_matrix_log_postproc_abs_shift = np.fft.fftshift(fft2_matrix_log_postproc, )
    plt.figure()
    plt.imshow(fft2_matrix_log_postproc_abs_shift, extent = extent_values, cmap = 'jet', aspect = 7)
    plt.colorbar()
    plt.xlabel('Range [m]')
    plt.ylabel('Velocity [m/s]')
    plt.title('Doppler-Range Heatmap ' + 'rx = %d' % r + ' and ' + 'tx = %d' % t)  
    plt.show()  
 
def plot_1d_fft(radar_data_cube, radar_data_conf):    
    #apply 1d-fft on each chirp
    for t in range(radar_data_conf['num_txs']):
        for r in range(radar_conf_info['num_txs']):
            for d in range(radar_conf_info['doppler_fft_size']):
                data_to_plot = radar_data_cube[r][d][t][:]
                fig, axs = plt.subplots(2, 1)
                axs[0].plot(data_to_plot.real, label = 'real chirp data')
                axs[0].plot(data_to_plot.imag, label = 'imag chirp data')
                axs[1].plot(abs(fft.fft(data_to_plot)), label = 'python fft')

def apply_python_fp_fft2(radar_data_matrix, fft2_fp_parameters):
    range_grow_logic = fft2_fp_parameters['range_grow_logic']
    doppler_grow_logic = fft2_fp_parameters['doppler_grow_logic']
    doppler_fft_size, range_fft_size = radar_data_matrix.shape[0], radar_data_matrix.shape[1]    
    radar_data_matrix_float = np.zeros((doppler_fft_size, range_fft_size), dtype = complex) 
    
    for d in range(doppler_fft_size):
        for s in range(range_fft_size):        
            radar_data_matrix_float[d][s] = complex(float(radar_data_matrix[d][s].real/pow(2,10)), float(radar_data_matrix[d][s].imag/pow(2,10))) 
    
    range_fft_matrix, py_model_fft2 = fft2radix2DIF(radarDataMatrix = radar_data_matrix_float, 
                                rangeGrowLogic = range_grow_logic,
                                dopplerGrowLogic = doppler_grow_logic, 
                                isFp = True)
    
    for r in range(range_fft_size): 
        for d in range(doppler_fft_size):
            real = int(py_model_fft2[d, r].real * pow(2, 10)) 
            imag = int(py_model_fft2[d, r].imag * pow(2, 10))
            rangeReal =  int(range_fft_matrix[d, r].real * pow(2, 10)) 
            rangeImag =  int(range_fft_matrix[d, r].imag * pow(2, 10)) 
            range_fft_matrix[d, r] = complex(rangeReal, rangeImag)
            py_model_fft2[d, r] = complex(real, imag)
    
    return py_model_fft2

# this is for 192
def flatten_fft2_data_cube(fft2_data_cube, mode):
    num_rxs, num_txs = fft2_data_cube_py_model.shape[0], fft2_data_cube_py_model.shape[2]
    range_fft_size, doppler_fft_size =  fft2_data_cube_py_model.shape[3], fft2_data_cube_py_model.shape[1]
    i = 0
    flatten_data = np.zeros(num_rxs*num_txs*range_fft_size*doppler_fft_size, dtype = complex)                                               
    for s in range(range_fft_size):
        for t in range(num_txs):
            for r in range(num_rxs):
                for d in range(doppler_fft_size):
                    flatten_data[i] = fft2_data_cube[r, d, t, s] # some kind of flatten function
                    i = i+1 
    return flatten_data

def check_stream_data_equality(data_array0, data_array1):
    # first way
    # data_count = 0
    # for i in range(len(data_array0)):
    #     if data_array0[i] == data_array1[i]:
    #         data_count = data_count + 1        
    # if (data_count == len(data_array0)):
    #     return True
    # else: 
    #     return False
    # second way
    return data_array0.all() == data_array1.all()
        
    
file_name_in = "gen_data_dir/iladata_radar_input_1.csv"
file_name_out = "gen_data_dir/iladata_radar_out_1.csv"

input_ila_data =  pd.read_csv(file_name_in, delimiter=',', usecols = ['design_1_i/axis_data_fifo_0_m_axis_tvalid', 'design_1_i/axis_data_fifo_0_m_axis_tdata[31:0]'], header=0, skiprows=0, )
axis_valid_column_in = input_ila_data['design_1_i/axis_data_fifo_0_m_axis_tvalid'].tolist()
axis_data_column_in = input_ila_data['design_1_i/axis_data_fifo_0_m_axis_tdata[31:0]'].tolist()

del axis_valid_column_in[0]
del axis_data_column_in[0]

output_ila_data =  pd.read_csv(file_name_out, delimiter=',', usecols = ['design_1_i/probe0', 'design_1_i/fft_2d_0_m_axis_TDATA[31:0]'], header=0, skiprows=0, )
axis_valid_column_out = output_ila_data['design_1_i/probe0'].tolist()
axis_data_column_out = output_ila_data['design_1_i/fft_2d_0_m_axis_TDATA[31:0]'].tolist()

del axis_valid_column_out[0]
del axis_data_column_out[0]

radar_conf_info = {'num_rxs': 4, 'num_txs': 3, 'range_fft_size': 256, 'doppler_fft_size': 32}
num_rxs = radar_conf_info['num_rxs']
num_txs = radar_conf_info['num_txs']
range_fft_size = radar_conf_info['range_fft_size'] 
doppler_fft_size = radar_conf_info['doppler_fft_size'] 

total_in_data = num_rxs * num_txs * range_fft_size * doppler_fft_size
total_out_data = total_in_data + 512
file_name_out = "gen_data_dir/complex_data_out_hex.txt"
file_name_in = "gen_data_dir/complex_data_in_hex.txt"

write_data_to_file(file_name_out, axis_data_column_out[:total_out_data])
write_data_to_file(file_name_in, axis_data_column_in[:total_in_data])


# ################################################
# radar configuration
Fc = 77e9
adc_res = 12
Tc = 75e-6      #73.7e-6 
Tc_idle = 50e-6 #7.5e-6
Fs = 4116e3     #7400e3
S = 50.018e12   #47.49e12
n_frames = 1
Bw = S * Tc

#################################################
# calculate range and velocity resolution
c0 = 299792458
lamb = c0/Fc
Tc_total = (Tc + Tc_idle) * num_txs                                      
v_max = lamb/4/Tc_total
v_res = 2*v_max/doppler_fft_size
velocities = np.arange(-v_res*doppler_fft_size/2, v_res*doppler_fft_size/2, v_res)
r_res = c0 / (2 * S * (range_fft_size/Fs))
distances = np.arange(0, r_res*range_fft_size, r_res)
extent_values = [np.min(distances), np.max(distances), np.min(velocities), np.max(velocities)]
isLog = False

##################################################
## fft2 parameters
NstagesRange = 8
growStagesRange = 4 
NstagesDoppler = 5
growStagesDoppler = 0

qformatRangeIn =  {'signed': True, 'm': 2, 'n': 10, 'rounding': 'convergent', 'overflow': 'clamp', 'overflow_alert': 'ignore'}
qformatDopplerIn = {'signed': True, 'm': 6, 'n': 10, 'rounding': 'convergent', 'overflow': 'clamp', 'overflow_alert': 'ignore'}
qformatRangeTwiddle = {'signed': True, 'm': 2, 'n': 14, 'rounding': 'convergent', 'overflow': 'clamp', 'overflow_alert': 'ignore'}
qformatDopplerTwiddle = {'signed': True, 'm': 2, 'n': 14, 'rounding': 'convergent', 'overflow': 'clamp', 'overflow_alert': 'ignore'}
rangeGrowLogic = np.concatenate((np.ones(growStagesRange, dtype = int), np.zeros(int(NstagesRange) - growStagesRange, dtype = int)))
dopplerGrowLogic = np.concatenate((np.ones(growStagesDoppler, dtype = int), np.zeros(int(NstagesRange) - growStagesDoppler, dtype = int)))
fft2_fp_parameters = {'range_grow_logic': rangeGrowLogic, 'doppler_grow_logic': dopplerGrowLogic}

radar_data_cube = repack_raw_data(axis_data_column_in[:total_in_data], radar_conf_info)
fft2_flatten_data, fft2_data_cube = repack_fft2_data(axis_data_column_out[:total_out_data], radar_conf_info, mode = 192)
fft2_data_cube_py_model = np.zeros((num_rxs, doppler_fft_size, num_txs, range_fft_size), dtype = complex) 


for t in range(1):
    for r in range(1):
        radar_data_matrix = radar_data_cube[r,:,t,:]
        fft2_data_cube_py_model[r, :, t, :] = apply_python_fp_fft2(radar_data_matrix, fft2_fp_parameters)
        plot_fft2_linear(fft2_data_cube_py_model[r, :, t, :], extent_values, r, t)

expected_data = flatten_fft2_data_cube(fft2_data_cube_py_model, 192)
insert_header_and_write_data_to_file(expected_data, 192, range_fft_size, 'gen_data_dir/complex_data_out_hex_py.txt')

if check_stream_data_equality(expected_data, fft2_flatten_data):
    print("All data are ok")

for m in range(num_rxs*num_txs):
    r = int(m % num_rxs)
    t = int(m / num_rxs)
    print(r)
    print(t)
    fft2_data_matrix_fpga = fft2_data_cube[m][:][:]
    plot_fft2_linear(fft2_data_matrix_fpga, extent_values, r, t)


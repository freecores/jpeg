 - Description

This project features a complete JPEG Hardware Compressor (standard Baseline DCT, JFIF header) with 2:1:1 subsampling, able to compress at a rate of up to 24 images per second (on XC2V1000-4 @ 40 MHz with CIF resolution: 352x288). 

IMAGE RESOLUTION IS LIMITED TO 352x288. It takes an RGB input (row-wise) and outputs to a memory the compressed JPEG image.

A testbench has been made that takes a bitmap image from your computer and writes a compressed JPEG file by simulating the code.

In order to be able to run the project you must first generate the RAM/ROM cores and the DCT2D core with Xilinx CoreGen. The configuration values are listed at the bottom of the file compressor.vhd.

If you run into any problems downloading the files from the cvs please check that you are downloading them in binary form. For any questions my email is: 
victor.lopez [(at)] ono [(dot)] com

PLEASE NOTICE THAT THIS CORE IS LICENSED AS GPL. That means you may use it only for NON-COMMERCIAL purposes.

- Directory Organization


   cores - Xilinx CoreGen generated cores are inside this directory (if it is not there anymore then you should be able to generate them using CoreGen with the parameters described in the section "Functional Description" of this document.)
   docs - documents about this project
   images - contains main test image to run testbench against
   src - contains main source file compressor.vhd
   testbench - contains main testbench file


- Description

This project features a complete JPEG Hardware Compressor (standard Baseline DCT, JFIF header) with 2:1:1 subsampling, able to compress at a rate of up to 24 images per second (on XC2V1000-4 @ 40 MHz with CIF resolution: 352x288). 

IMAGE RESOLUTION IS LIMITED TO 352x288. It takes an RGB input (row-wise) and outputs to a memory the compressed JPEG image.

A testbench has been made that takes a bitmap image from your computer and writes a compressed JPEG file by simulating the code.

Anyone interested in the image standards used in this project, they can be downloaded from the following places:

JPEG (ITU-T81 standard): http://www.w3.org/Graphics/JPEG/itu-t81.pdf 
JFIF (JPEG file headers): http://www.w3.org/Graphics/JPEG/jfif3.pdf 
BMP (bitmaps for the testbench): http://netghost.narod.ru/gff/vendspec/micbmp/bmp.txt

If you run into any problems downloading the files from the cvs please check that you are downloading them in binary form. For any questions my email is: 
victor.lopez [(at)] ono [(dot)] com

PLEASE NOTICE THAT THIS CORE IS LICENSED AS GPL. That means you may use it only for NON-COMMERCIAL purposes.

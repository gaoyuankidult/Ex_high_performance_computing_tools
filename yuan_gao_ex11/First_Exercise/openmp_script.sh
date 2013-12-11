#!/bin/bash                                                                                      

ITERATION=5000
PARTICAL=$1
PRINT=$2
THREAD=$3

# 500 

clear && gfortran md1d_openmp.f90 &&  ./a.out $PARTICAL 0.01 $ITERATION 1 $PRINT 0 $THREAD
gfortran -fopenmp md1d_openmp.f90 &&  ./a.out $PARTICAL 0.01 $ITERATION 1 $PRINT 0 $THREAD




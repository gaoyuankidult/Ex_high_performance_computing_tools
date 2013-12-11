#!/bin/bash                                                                                      

ITERATION=500
PARTICAL=$1
PRINT=$2
THREAD=$3

clear && gfortran md1d_openmp.f90 &&  ./a.out $PARTICAL 0.01 $ITERATION 1 $PRINT 0 $THREAD
mpif90 md1d_mpi.f90  && mpirun -np $THREAD  ./a.out $PARTICAL 0.01 $ITERATION 1 $PRINT 0 
gfortran -fopenmp md1d_openmp.f90 &&  ./a.out $PARTICAL 0.01 $ITERATION 1 $PRINT 0 $THREAD




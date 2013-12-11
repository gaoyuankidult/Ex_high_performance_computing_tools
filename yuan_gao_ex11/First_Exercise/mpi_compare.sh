#!/bin/bash                                                                                      


PARTICAL=$1
PRINT=$2
STEP=$3
ITERATION=$4
THREAD=$5

clear && mpif90 md1d_mpi_v2.f90 -o md1d_mpi_v2 && mpirun -np $THREAD  ./md1d_mpi_v2 $PARTICAL 0.01 $ITERATION $STEP $PRINT 1 
mpif90 md1d_nompi.f90 -o md1d_nompi && mpirun -np 1 ./md1d_nompi $PARTICAL 0.01 $ITERATION $STEP $PRINT 1 

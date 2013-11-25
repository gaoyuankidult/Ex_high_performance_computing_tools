#include <stdio.h>
#include "mpi.h"

int main( int argc, char **argv )
{
    int send[4][4], recv[4][4];
    int rank, nprocess, k,l;
    MPI_Init( &argc, &argv );
    MPI_Comm_rank( MPI_COMM_WORLD, &rank );
    MPI_Comm_size( MPI_COMM_WORLD, &nprocess );
    if(nprocess != 4)
    {
      printf("I need to fix the number of using processors!\n It has to be 4 !");
      
    }
    for(l=0;l<nprocess;l++){
      for (k=0;k<nprocess;k++) send[l][k] = (k+1) + nprocess + l*5;
    }

    MPI_Alltoall(&send[rank], 1, MPI_INT, &recv[rank], 1, MPI_INT, MPI_COMM_WORLD);
    printf("recv vector of process %d: %d %d %d %d\n", rank, recv[rank][0], recv[rank][1], recv[rank][2], recv[rank][3]);
    if(rank == 3)
    {
      printf("\n\n");
      for(k = 0; k < 4; k++)
      {
        printf("send vector of process %d: %d %d %d %d\n", rank, send[k][0], send[k][1], send[k][2], send[k][3]);
      }
    }
    MPI_Finalize();
    return 0;
}


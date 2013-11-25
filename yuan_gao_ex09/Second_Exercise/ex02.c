#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

int main(int argc, char** argv)
{
	double t0,t1,timeused;
t0=MPI_Wtime();
    int size, rank;

    MPI_Init(&argc,&argv);
    MPI_Comm_size(MPI_COMM_WORLD,&size);
    MPI_Comm_rank(MPI_COMM_WORLD,&rank);

    int localsum[1] = {0};
    int globalsum[1] = {0};
t0=MPI_Wtime();

    if( rank > 0 )
    {
        localsum[0] += 10;
    }

    MPI_Reduce(localsum,globalsum,2,MPI_INT,MPI_SUM,0,MPI_COMM_WORLD);

    if(rank==0)
    {
        printf("globalsum1 = %d \n",globalsum[0]);
    }

    MPI_Finalize();
	t1=MPI_Wtime();
	printf("Time consumed :%f\n",t1-t0);
    return (EXIT_SUCCESS);
}

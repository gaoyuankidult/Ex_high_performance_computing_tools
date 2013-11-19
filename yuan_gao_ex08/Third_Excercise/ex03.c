#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>
#include <assert.h>


int main(int argc, char** argv) {
  if (argc != 2) {
    fprintf(stderr, "Usage: compare_bcast num_elements num_trials\n");
    exit(1);
  }

  int num_elements = atoi(argv[1]);
  if(num_elements < 0)
    {
      printf("The value received is negative. Abort process.\n");
      exit(1);
    }
  int root = 0;


  MPI_Init(NULL, NULL);
  int world_rank;
  MPI_Comm_rank(MPI_COMM_WORLD, &world_rank);
  int* data = (int*)malloc(sizeof(int) * 2);
  data[0] = num_elements;
  data[1] = 1;
  assert(data != NULL);
  MPI_Bcast(data, num_elements, MPI_INT, root, MPI_COMM_WORLD);
  if(world_rank != 0)
  {
    printf("The data %d is received on process %d.\n",data[0],world_rank);
  }

  MPI_Finalize();
}

#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>

main(int argc, char* argv[]) {
  int rank,nprocs;
  MPI_Status status;
  MPI_Request request;
  int done,myfound, nvalues;
  int svalue = 11;
  int nmax = 400;
  int b[nmax];
  int *sub;
  int i,j,dummy,index;
  FILE *infile;
  MPI_Init(&argc,&argv);
  MPI_Comm_rank(MPI_COMM_WORLD,&rank);
  MPI_Comm_size(MPI_COMM_WORLD,&nprocs);
  myfound=0;
  if (rank==0) {
    infile=fopen("file.dat","r");
    for(i=0;i<nmax;++i) {
      fscanf(infile,"%d",&b[i]);
    }
  }
  nvalues=nmax/nprocs;
  sub = (int *)malloc(nvalues *sizeof(int));
  MPI_Scatter(b,nvalues,MPI_INT,sub,nvalues,MPI_INT,0,MPI_COMM_WORLD);
  MPI_Barrier(MPI_COMM_WORLD); 
  MPI_Irecv(&dummy,1,MPI_INT,MPI_ANY_SOURCE,86,MPI_COMM_WORLD,&request);
  MPI_Test(&request,&done,&status);
  i=0;
  while(!done && i<nvalues) {
    if (sub[i]==svalue) {
      dummy=123;
      for(j=0;j<=nprocs-1;++j) { 
         MPI_Send(&dummy,1,MPI_INT,j,86,MPI_COMM_WORLD);
      }
      printf("Process:%d found it at local index %d\n",rank,i);
      myfound=1;
    }
    MPI_Test(&request,&done,&status);
    ++i;
  }
  if (!myfound) {
    if (i==0) 
      index=0;
    else
      index=i-1;
    printf("Process:%d stopped at local index %d\n",rank,index);
  }
   MPI_Finalize();
  return 0;
	  
}

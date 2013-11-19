#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

main(int argc,char *argv[])
{
  const int tag=50;
  int id,ntasks,source_id,dest_id,rc,i;
  MPI_Status status;
  int msg[2],pnlen;
  int recv_msg[2];
  char pname[MPI_MAX_PROCESSOR_NAME];

  rc=MPI_Init(&argc,&argv);
  if (rc != MPI_SUCCESS) {
    printf("MPI initialization failed\n");
    exit(1);
  }

  // When sending data every process prints out its own and the recipient’s id-numbers.
  rc=MPI_Comm_size(MPI_COMM_WORLD,&ntasks);
  rc=MPI_Comm_rank(MPI_COMM_WORLD,&id);
  rc=MPI_Get_processor_name(pname,&pnlen);

  if (id != ntasks-1) {
    msg[0]=id;
    msg[1]=0;
    dest_id=id+1;
    //The message is an array with the first element containing the id-number of the sending process.
    rc=MPI_Send(msg,2,MPI_INT,dest_id,tag,MPI_COMM_WORLD);
    printf("sending message : %d  sending to: %d from: %d\n",msg[0],dest_id,id);

   }
  if(id!=0)
    {
      //When receiving a message every process prints out the sender id-number and the first element of the message array.
      rc=MPI_Recv(recv_msg,2,MPI_INT,MPI_ANY_SOURCE,tag,MPI_COMM_WORLD,&status);
      source_id=status.MPI_SOURCE;
      printf("first received message is: %d sender: %d process: %d\n\n",recv_msg[0],source_id,id);
    }
  else
    {
      printf("\n");
    }
  rc=MPI_Finalize();
  exit(0);
}

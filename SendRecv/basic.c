#include<stdio.h>
#include<stdlib.h>
#include<mpi.h>

int main(int argc,char *argv[])
{
    int i,myid, ntasks;
    int size=1000000;
    int *message;
    MPI_Status status;
    MPI_Init(&argc,&argv);
    MPI_Comm_size(MPI_COMM_WORLD,&ntasks);
    MPI_Comm_rank(MPI_COMM_WORLD,&myid);
    
    //allocate message 
    message = malloc(sizeof(int)*size);
    //initialize message
    for(i=0;i<size;i++) message[i]=myid;
    
    //send and receive messages as defined in exercise
    
    free(message);
    
    MPI_Finalize();
}

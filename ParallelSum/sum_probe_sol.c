#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>
#include <time.h>

int main(int argc, char *argv[]){
    int i,rank,nprocs, N,Nloc,MAX_NUMBERS;
    double *array;
    double sum,psum;
    MPI_Status status;
    
    MPI_Init(&argc,&argv);
    MPI_Comm_rank(MPI_COMM_WORLD,&rank);
    MPI_Comm_size(MPI_COMM_WORLD,&nprocs);
    MAX_NUMBERS=100;
    srand(time(NULL));
    // Pick a random amount of integers to send to process one
    N = (rand() / (float)RAND_MAX) * MAX_NUMBERS;

    if(rank==0){
        Nloc=N/3;
        array=malloc(sizeof(double)*N);
        for(i=0;i<N;i++) array[i]=1.0;
        // Send the amount of doubles to process one
        MPI_Send(array+Nloc,N-Nloc,MPI_DOUBLE,1,10,MPI_COMM_WORLD);
		fprintf(stderr,"Task 0 sent %d numbers to Task 1\n", N-Nloc);
    }
    else{
        // Probe for an incoming message from process zero
        MPI_Probe(0, MPI_ANY_TAG, MPI_COMM_WORLD, &status);
        // When probe returns, the status object has the size and other
        // attributes of the incoming message. Get the size of the message.
        MPI_Get_count(&status,MPI_DOUBLE,&Nloc);
        fprintf(stderr,"Task 1 dynamically received %d numbers from Task 0.\n", Nloc);
        // Allocate a buffer just big enough to hold the incoming numbers
        array=malloc(sizeof(double)*Nloc);
        // Now receive the message with the allocated buffer
        MPI_Recv(array,Nloc,MPI_DOUBLE,0,10,MPI_COMM_WORLD,&status);
        fprintf(stderr,"rank = %d Nloc =%d Message source = %d\n",rank,Nloc);
    }
    psum=0;
    for(i=0;i<Nloc;i++){
        psum+=array[i];
    }
    
    if(rank==0) {
        MPI_Recv(&sum,1,MPI_DOUBLE,1,11,MPI_COMM_WORLD,MPI_STATUS_IGNORE);
        sum+=psum;
        printf("Sum is %g, partial sum %g\n",sum,psum);
    }
    else {
        MPI_Send(&psum,1,MPI_DOUBLE,0,11,MPI_COMM_WORLD);
        printf("Sum is %g, partial sum %g\n",sum,psum);
    }
    MPI_Finalize();
}

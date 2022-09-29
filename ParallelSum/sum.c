#include <mpi.h>
#include <stdio.h> 
#include <stdlib.h> 
int main(int argc, char *argv[]){ 
    int i,N,rank,nprocs; 
    double *array; 
    double sum,psum,fsum; 

    MPI_Init(&argc,&argv);
    MPI_Comm_rank(MPI_COMM_WORLD,&rank);
    MPI_Comm_size(MPI_COMM_WORLD,&nprocs);

    N=100; 
    array=(double *)calloc(sizeof(double),N); 

    if (rank==0) {
    	for(i=0;i<N;i++){ 
        	array[i]=1.0; 
    	} 
	// send N/2 data points to 1
    	MPI_Send(array+N/2,N/2,MPI_DOUBLE,1,10,MPI_COMM_WORLD);
    }
    else {
	// receive N/2 data points from 0
    	MPI_Recv(array,N,MPI_DOUBLE,0,10,MPI_COMM_WORLD,MPI_STATUS_IGNORE);
    }

    sum=0; 
    for(i=0;i<N/2;i++){ 
        sum+=array[i]; 
    } 

    if (rank==0) {
    	MPI_Recv(&sum,1,MPI_DOUBLE,1,11,MPI_COMM_WORLD,MPI_STATUS_IGNORE);
	fsum = sum + psum;
        printf("final Sum is %g\n",fsum); 
    }
    else{
    	MPI_Send(&sum,1,MPI_DOUBLE,0,11,MPI_COMM_WORLD);
    }

    MPI_Finalize();
}

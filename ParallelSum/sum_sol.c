#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

int main(int argc, char *argv[]){
    int i,rank,nprocs, N;
    double *array;
    double sum,psum;
    MPI_Init(&argc,&argv);
    MPI_Comm_rank(MPI_COMM_WORLD,&rank);
    MPI_Comm_size(MPI_COMM_WORLD,&nprocs);

    N=100;

    if(rank==0){
        array=(double *)malloc(sizeof(double)*N);
        for(i=0;i<N;i++) array[i]=1.0;
        MPI_Send(array+N/2,N/2,MPI_DOUBLE,1,10,MPI_COMM_WORLD);
    }
    
    else{
        array=(double *)malloc(sizeof(double)*N);
        MPI_Recv(array,N/2,MPI_DOUBLE,0,10,MPI_COMM_WORLD,MPI_STATUS_IGNORE);
    }

    psum=0;
    for(i=0;i<N/2;i++){
        psum+=array[i];
    }
    
    if(rank==0) {
        MPI_Recv(&sum,1,MPI_DOUBLE,1,11,MPI_COMM_WORLD,MPI_STATUS_IGNORE);
        sum+=psum;
        fprintf(stderr,"Sum is %f, partial sum %f\n",sum,psum);
    }
    else {
        MPI_Send(&psum,1,MPI_DOUBLE,0,11,MPI_COMM_WORLD);
    }
    MPI_Finalize();
}







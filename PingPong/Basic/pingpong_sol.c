#include<stdio.h>
#include<stdlib.h>
#include<mpi.h>

int main(int argc,char *argv[])
{
    int i,myid, ntasks;
    int size;;
    double *message;
    int ping=101;
    int pong=102;
    double t1,t2;
    
    MPI_Status status;
    MPI_Init(&argc,&argv);
    MPI_Comm_size(MPI_COMM_WORLD,&ntasks);
    MPI_Comm_rank(MPI_COMM_WORLD,&myid);

    message=malloc(sizeof(double)*100001);
    
    if(myid==0){
        for(size=10;size<100001;size+=1000){
            t1=MPI_Wtime();
            for(i=0;i<10;i++){
                MPI_Send(message,size,MPI_DOUBLE,1,ping,MPI_COMM_WORLD);
                MPI_Recv(message,size,MPI_DOUBLE,1,pong,MPI_COMM_WORLD,MPI_STATUS_IGNORE);
            }
            t2=MPI_Wtime();
            printf("Size %d (bytes) Time %g (s) Bandwidth %g bytes/s\n",size*sizeof(double),(t2-t1)/10,2*sizeof(double)*10*size/(t2-t1));
        }
    }
    if(myid==1){
        for(size=10;size<100001;size+=1000){
            for(i=0;i<10;i++){
                MPI_Recv(message,size,MPI_DOUBLE,0,ping,MPI_COMM_WORLD,MPI_STATUS_IGNORE);
                MPI_Send(message,size,MPI_DOUBLE,0,pong,MPI_COMM_WORLD);
                
            }
        }
    }
    free(message);
    
    MPI_Finalize();
}

#include<stdio.h>
#include<stdlib.h>
#include<math.h>
#include<mpi.h>

int main(int argc,char *argv[])
{
    int myid, ntasks, n, m, i,j, count;
    const int maxN=1000000;
    double *outbuf, *inbuf;
    MPI_Status status;
    
    MPI_Init(&argc,&argv);
    
    MPI_Comm_size(MPI_COMM_WORLD,&ntasks);
    MPI_Comm_rank(MPI_COMM_WORLD,&myid);
    outbuf=malloc(sizeof(double)*maxN);
    inbuf=malloc(sizeof(double)*maxN);
    
    for (n=1; n<=maxN;n*=10){
        if (myid == 0){
            for(i=0; i<n; i++) outbuf[i]=0;
            for(j=1;j<ntasks;j++){
                MPI_Send(outbuf,n,MPI_DOUBLE,j,0,MPI_COMM_WORLD);
                MPI_Recv(inbuf,n,MPI_DOUBLE,j,1,MPI_COMM_WORLD,&status);
                MPI_Get_count(&status,MPI_DOUBLE,&count);
                printf("0 received %d numbers\n",count);
            }
        }
        else {
            for(i=0; i<n; i++) outbuf[i]=1;
            MPI_Send(outbuf,n,MPI_DOUBLE,0,1,MPI_COMM_WORLD);
            MPI_Recv(inbuf,n,MPI_DOUBLE,0,0,MPI_COMM_WORLD,&status);      
            MPI_Get_count(&status,MPI_DOUBLE,&count);
            printf("%d received %d numbers\n",myid,count);
        }     
    }
    free(outbuf);
    free(inbuf);
    MPI_Finalize();
}

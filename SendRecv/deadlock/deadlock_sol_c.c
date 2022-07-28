#include<stdio.h>
#include<stdlib.h>
#include<math.h>
#include<mpi.h>

int main(int argc,char *argv[])
{
    int myid, ntasks, n, m, i,j, count;
    const int maxN=1000000;
    double *outbuf, *inbuf;
    MPI_Status status[2];
    MPI_Request req[2];

    MPI_Init(&argc,&argv);
    MPI_Comm_size(MPI_COMM_WORLD,&ntasks);
    MPI_Comm_rank(MPI_COMM_WORLD,&myid);

    outbuf=malloc(sizeof(double)*maxN);
    inbuf=malloc(sizeof(double)*maxN);
    
    for (n=1; n<=maxN;n*=10){
        if (myid == 0){
            for(i=0; i<n; i++) outbuf[i]=0;
            for(j=1;j<ntasks;j++){
                MPI_Irecv(inbuf,n,MPI_DOUBLE,j,1,MPI_COMM_WORLD,req);
                MPI_Isend(outbuf,n,MPI_DOUBLE,j,0,MPI_COMM_WORLD,req+1);
                MPI_Waitall(2,req,status);
                MPI_Get_count(&(status[0]),MPI_DOUBLE,&count);
                fprintf(stderr,"0 received %d numbers\n",count);
            }
        }
        else {
            for(i=0; i<n; i++) outbuf[i]=1;
            MPI_Irecv(inbuf,n,MPI_DOUBLE,0,0,MPI_COMM_WORLD,req);      
            MPI_Isend(outbuf,n,MPI_DOUBLE,0,1,MPI_COMM_WORLD,req+1);
            MPI_Waitall(2,req,status);
            MPI_Get_count(&(status[0]),MPI_DOUBLE,&count);
            fprintf(stderr,"%d received %d numbers\n",myid,count);
        }     
    }
    fprintf(stderr, "MPI rank %d is ready with Send/Recv\n",myid);
    free(outbuf);
    free(inbuf);
    MPI_Finalize();
}

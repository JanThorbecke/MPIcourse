#include<stdlib.h>
#include<stdio.h>
#include<math.h>
#include<mpi.h>

int main(int argc,char *argv[])
{
  int myid, ntasks, n, m, i, count;
  MPI_Status status;
  const int maxN=1000000;
  double *outbuf, *inbuf;
    
  MPI_Init(&argc,&argv);
  MPI_Comm_size(MPI_COMM_WORLD,&ntasks);
  MPI_Comm_rank(MPI_COMM_WORLD,&myid);

  outbuf=malloc(sizeof(double)*maxN);
  inbuf=malloc(sizeof(double)*maxN);

  for (n=1; n<=maxN;n*=10){
    if (myid == 0){
      for(i=0; i<n; i++)
	outbuf[i]=0;
      for(i=1;i<ntasks;i++){
          MPI_Sendrecv(outbuf,n,MPI_DOUBLE,i,0,
                       inbuf,n,MPI_DOUBLE,i,1,MPI_COMM_WORLD,&status);
          MPI_Get_count(&status,MPI_DOUBLE,&count);
          printf("0 received %d numbers\n",count);
      }
    }
    else {
        for(i=0; i<n; i++)
            outbuf[i]=1;
        MPI_Sendrecv(outbuf,n,MPI_DOUBLE,0,1,
                 inbuf,n,MPI_DOUBLE,0,0,MPI_COMM_WORLD,&status);      
        
        MPI_Get_count(&status,MPI_DOUBLE,&count);
        printf("1 received %d numbers\n",count);
    }     
  }
  free(outbuf);
  free(inbuf);
  MPI_Finalize();
}

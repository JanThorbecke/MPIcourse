#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <mpi.h>

int main(int argc,char *argv[]){
    int i, myid, nproc;
    int *buf;
    int bufsize;
    
    MPI_Init(&argc,&argv);
    MPI_Comm_rank(MPI_COMM_WORLD,&myid);
    MPI_Comm_size(MPI_COMM_WORLD,&nproc);

    bufsize = 100 / nproc;
    assert(bufsize * nproc == 100);
    buf = (int *) malloc(bufsize * sizeof(int));

    int *readbuf;
    int *commbuf;
    int *orig_commbuf;
    int *orig_readbuf;
    int *swap;
    MPI_Request request;
    FILE *fp;

    if(myid==0) {
        fp=fopen("test.dat","r");
	commbuf = (int *) malloc(bufsize * sizeof(int));
	readbuf = (int *) malloc(bufsize * sizeof(int));
	fread(buf, sizeof(int), bufsize, fp);
	fread(commbuf, sizeof(int), bufsize, fp);
	for(int i=1;i<nproc-1;i++) {
	  MPI_Isend(commbuf, bufsize, MPI_INT, i, 1, MPI_COMM_WORLD, &request);
	  fread(readbuf, sizeof(int), bufsize, fp);
	  /*wait for send to rank i to end */
	  MPI_Wait(&request,MPI_STATUS_IGNORE);
	  /*swap writebuf and commbuf*/
	  swap = readbuf;
	  readbuf = commbuf;
	  commbuf = swap;
        }
	MPI_Send(commbuf, bufsize, MPI_INT, nproc-1, 1, MPI_COMM_WORLD);
        free(commbuf);
        free(readbuf);
	fclose(fp);
    }
    else {
      /*receive data from rank 0 */
      MPI_Recv(buf, bufsize, MPI_INT, 0, 1, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
    }

    /* Print out the data for checking */
    printf("Rank %d, first and last elements obtained: %d %d\n", myid, 
	   buf[0], buf[bufsize-1]);

    MPI_Finalize();

}

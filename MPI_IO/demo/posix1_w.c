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
    for (int i=0; i < bufsize; i++)
      buf[i] = myid*bufsize + i;

    
    int *writebuf;
    int *commbuf;
    int *orig_commbuf;
    MPI_Request request;
    FILE *fp;

    if(myid==0) {
        fp=fopen("test.dat","w");
	commbuf = (int *) malloc(bufsize * sizeof(int));
	orig_commbuf=commbuf; /*store original pointer so that it can easily be freed */
	writebuf = buf;
	for(int i=1;i<nproc;i++) {
	  MPI_Irecv(commbuf, bufsize, MPI_INT, i, 1, MPI_COMM_WORLD, &request);
	  fwrite(writebuf,sizeof(int),bufsize,fp);
	  /*wait for send from rank i to end */
	  MPI_Wait(&request,MPI_STATUS_IGNORE);
	  /*swap writebuf and commbuf*/
	  buf = writebuf;
	  writebuf = commbuf;
	  commbuf = buf;
        }
	fwrite(writebuf, sizeof(int), bufsize, fp);
        free(orig_commbuf);
	fclose(fp);
    }
    else {
	/*send data to rank 0, ssend to avoid overloading root process */
        MPI_Ssend(buf, bufsize, MPI_INT, 0, 1, MPI_COMM_WORLD);
    }

    MPI_Finalize();

}

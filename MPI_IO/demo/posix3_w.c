#include <stdio.h>
#include <stdlib.h>
#include <math.h>
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

    int procs_per_writer;
    int io_myid,io_nproc;
    MPI_Comm io_comm;
    FILE *fp;
    char filename[20];
    MPI_Request request;
    
    int *writebuf;
    int *commbuf;
    int *orig_commbuf;

    procs_per_writer=nproc/sqrt((double)nproc);
    MPI_Comm_split(MPI_COMM_WORLD,myid/procs_per_writer,0,&io_comm);
    MPI_Comm_rank(io_comm,&io_myid);
    MPI_Comm_size(io_comm,&io_nproc);

    /* Open file and initialize buffers */
    if(io_myid==0) {
      sprintf(filename,"test_%04d.dat",myid/procs_per_writer);
      fp=fopen(filename,"w");
      commbuf = (int *) malloc(bufsize * sizeof(int));
      orig_commbuf=commbuf; /*store original pointer so that it can easily be freed */
      writebuf = buf;
      for(int i=1; i<io_nproc; i++) {
	MPI_Irecv(commbuf, bufsize, MPI_INT, i, 1, io_comm, &request);
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
      MPI_Ssend(buf, bufsize, MPI_INT, 0, 1, io_comm);
    }



    MPI_Finalize();

}

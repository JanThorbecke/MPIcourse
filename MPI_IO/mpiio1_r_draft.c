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

    MPI_File fp;

     /* Open an MPI I/O file for reading */

    /* Read the data */
    MPI_Offset disp;
    disp = myid*bufsize*sizeof(int);

    /* Close the file */

    /* Print out the data for checking */
    printf("Rank %d, first and last elements obtained: %d %d\n", myid,
           buf[0], buf[bufsize-1]);

    MPI_Finalize();
}

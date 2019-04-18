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

    MPI_File fp;

     /* Open an MPI I/O file for writing */
    MPI_File_open(MPI_COMM_WORLD, "test.dat",
                  MPI_MODE_CREATE|MPI_MODE_WRONLY,
                  MPI_INFO_NULL, &fp);

    MPI_Offset disp, fpointer;
    /* Initial file pointer */
    MPI_File_get_position(fp, &fpointer);
    printf("Rank %d, initial file pointer %d\n", myid, fpointer);
    disp = myid*bufsize*sizeof(int);
    MPI_File_seek(fp, disp, MPI_SEEK_SET);
    MPI_File_get_position(fp, &fpointer);
    printf("Rank %d, file pointer after seek %d\n", myid, fpointer);
    MPI_File_write(fp, buf, bufsize,
		   MPI_INT, MPI_STATUS_IGNORE);
    MPI_File_get_position(fp, &fpointer);
    printf("Rank %d, file pointer after write1 %d\n", myid, fpointer);

    /* Close the file */
    MPI_File_close(&fp);
    MPI_Finalize();
}

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
    MPI_File_open(MPI_COMM_WORLD, "test.dat",
                  MPI_MODE_RDONLY,
                  MPI_INFO_NULL, &fp);

#ifdef USE_FILE_SEEK
    /* Update the file pointer and read */
    MPI_Offset disp;
    disp = myid*bufsize*sizeof(int);
    MPI_File_seek(fp, disp, MPI_SEEK_SET);
    MPI_File_read(fp, buf, bufsize,
		  MPI_INT, MPI_STATUS_IGNORE);
#else
    /* Read the data collectively with explicit offset*/
    MPI_File_read_at_all(fp, myid*bufsize*sizeof(int),
			 buf,bufsize,MPI_INT,MPI_STATUS_IGNORE);
#endif
    /* Close the file */
    MPI_File_close(&fp);

    /* Print out the data for checking */
    printf("Rank %d, first and last elements obtained: %d %d\n", myid,
           buf[0], buf[bufsize-1]);

    MPI_Finalize();
}

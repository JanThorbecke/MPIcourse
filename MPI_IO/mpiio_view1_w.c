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
      buf[i] = myid + i * nproc;

    MPI_File fp;

     /* Open an MPI I/O file for writing */
    MPI_File_open(MPI_COMM_WORLD, "test.dat",
                  MPI_MODE_CREATE|MPI_MODE_WRONLY,
                  MPI_INFO_NULL, &fp);

    /* Create the file view */
    MPI_Datatype filetype;
    MPI_Offset disp;
    disp = myid * sizeof(int);
    MPI_Type_vector(bufsize, 1, nproc, MPI_INT, &filetype);
    MPI_Type_commit(&filetype);
    MPI_File_set_view(fp, disp, MPI_INT, filetype, "native", MPI_INFO_NULL);
    
    /* Write the data */
    MPI_File_write_all(fp, buf, bufsize, MPI_INT, MPI_STATUS_IGNORE);

    /* Free the filetype */
    MPI_Type_free(&filetype);
    
    /* Close the file */
    MPI_File_close(&fp);
    MPI_Finalize();
}

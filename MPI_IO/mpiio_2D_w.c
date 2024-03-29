#include <mpi.h>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#define nx 32

int main(int argc, char *argv[]) {

  int myid, nproc, i;

  int *buf;
  int bufsize;

  /* Variables for cartesian topology */
  int mpi_dims[2];
  int period[2] = {0, 0};
  int coords[2];
  MPI_Comm cart_comm;

  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &nproc);

  mpi_dims[0] = mpi_dims[1] = sqrt(nproc);
  assert(mpi_dims[0] * mpi_dims[1] == nproc);
  MPI_Cart_create(MPI_COMM_WORLD, 2, mpi_dims, period, 0, &cart_comm);
  MPI_Comm_rank(cart_comm, &myid);
  MPI_Cart_coords(cart_comm, myid, 2, coords);

  fprintf (stderr,"PE-%3d: x=%d, y=%d \n", myid, coords[0], coords[1]);

  /* Variables for domain decomposition and subarray view */
  int dimsf[2] = {nx, nx};
  int count[2];
  int offset[2];
  int ndims = 2;

  bufsize = 1;
  for (i=0; i < 2; i++)
    {
      count[i] = nx/mpi_dims[i];
      offset[i] = coords[i] * count[i];
      bufsize *= count[i];
    }

//	fprintf(stderr,"ID %d with coords (%d, %d) offset %d %d bufsize %d\n",myid,coords[0], coords[1], offset[0], offset[1], bufsize);
    buf = (int *) malloc(bufsize * sizeof(int));
    for (i=0; i < bufsize; i++)
       buf[i] = myid;

  MPI_File fp;
  MPI_Datatype filetype;

  /* Open the file */
  MPI_File_open(MPI_COMM_WORLD, "test.dat",
                  MPI_MODE_CREATE|MPI_MODE_WRONLY,
                  MPI_INFO_NULL, &fp);

  /* Create the subarray view */
  MPI_Type_create_subarray(ndims, dimsf, count, offset, MPI_ORDER_C, 
			   MPI_INT, &filetype);
  MPI_Type_commit(&filetype);
  MPI_File_set_view(fp, 0, MPI_INT, filetype, "native", MPI_INFO_NULL);

  /* Write the data */
  MPI_File_write_all(fp, buf, bufsize, MPI_INT, MPI_STATUS_IGNORE);

  /* Free filetype and close the file */
  MPI_Type_free(&filetype);
  MPI_File_close(&fp);

  MPI_Finalize();
}

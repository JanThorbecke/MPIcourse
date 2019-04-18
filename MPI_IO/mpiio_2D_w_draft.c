#include <mpi.h>
#include <math.h>
#include <stdlib.h>
#include <assert.h>

#define nx 32

int main(int argc, char *argv[]) {

  int myid, nproc;

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
  /* Create a cartesian 2D topology */
  MPI_Cart_create(MPI_COMM_WORLD, 2, mpi_dims, period, 0, &cart_comm);
  MPI_Comm_rank(cart_comm, &myid);
  MPI_Cart_coords(cart_comm, myid, 2, coords);


  /* Variables for domain decomposition and subarray view */
  int dimsf[2] = {nx, nx};
  int count[2];   /* Size of local array in each dimension */
  int offset[2];  /* Position of local array with respect to global array */
  int ndims = 2;

  /* Determine the count, offset and total buffer size */
  bufsize = 1;
  for (int i=0; i < 2; i++)
    {
      count[i] = 
      offset[i] = 
      bufsize *= count[i];
    }

  buf = (int *) malloc(bufsize * sizeof(int));
  for (int i=0; i < bufsize; i++)
    buf[i] = myid;

  MPI_File fp;
  MPI_Datatype filetype;

  /* Open the file */

  /* Create the subarray view */

  /* Write the data */

  /* Free filetype and close the file */

  MPI_Finalize();
}

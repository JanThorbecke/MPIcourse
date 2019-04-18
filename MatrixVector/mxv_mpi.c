#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

#define root 0
#define n 512

int main(int argc, char *argv[]){
  float *A, *Aloc;
  float *x, *y, *yloc;
  int rc, ntasks, my_id;
  int ndim, i, j, low, up;
  MPI_Init(&argc,&argv);
  MPI_Comm_size(MPI_COMM_WORLD,&ntasks);
  MPI_Comm_rank(MPI_COMM_WORLD,&my_id);

  ndim = n*ntasks;
  x=(float *)malloc(ndim*sizeof(float));
  y=(float *)malloc(ndim*sizeof(float));
  yloc=(float *)malloc(n*sizeof(float));
  Aloc= (float *)malloc(n*ndim*sizeof(float));
  A= (float *)malloc(ndim*ndim*sizeof(float));
  if (my_id == root) {
    for (i=0; i<ndim; i++){
      for (j=0; j<ndim; ++j)
        A[i*ndim+j] = (float)drand48();
        x[i] = (float)drand48();
    }
  } 

/*  Transfer the blocks of rows as well as the whole x to  */
/*  tasks */

  MPI_Scatter(A, n*ndim, MPI_FLOAT, Aloc, n*ndim, 
	      MPI_FLOAT, 0,MPI_COMM_WORLD);
  MPI_Bcast(x, ndim, MPI_FLOAT, root, MPI_COMM_WORLD);

  for (i=0; i<n; ++i){
    for (j=0; j<ndim; ++j) {
      yloc[i] = yloc[i] + Aloc[i*ndim+j]*x[j];
    }
  }

/*  Combine the full y and make it available to all tasks */

  MPI_Allgather(yloc, n, MPI_FLOAT, y, n, MPI_FLOAT, MPI_COMM_WORLD);

  if (my_id == root) for (i=0; i<ndim; ++i) printf("%f ", y[i]);
  
  free(A); 
  free(Aloc); 
    
  MPI_Finalize();

}



#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

#define root 0
#define n 512

int main(int argc, char *argv[]){
  float *A, *Aloc, sumy;
  float *x, *y, *yloc;
  int rc, ntasks, my_id, nblock;
  int ndim, i, j, low, up;
  MPI_Init(&argc,&argv);
  MPI_Comm_size(MPI_COMM_WORLD,&ntasks);
  MPI_Comm_rank(MPI_COMM_WORLD,&my_id);

  ndim = n*ntasks;
  nblock = n*ndim;
  x=(float *)malloc(ndim*sizeof(float));
  y=(float *)malloc(ndim*sizeof(float));
  yloc=(float *)calloc(n,sizeof(float));
  Aloc= (float *)malloc(nblock*sizeof(float));
  if (my_id == root) {
    A= (float *)malloc(ndim*ndim*sizeof(float));
    srand48(10);
    for (i=0; i<ndim; i++){
      for (j=0; j<ndim; ++j){
        A[i*ndim+j] = (float)drand48();
      }
      x[i] = (float)drand48();
    }
  } 

/*  Transfer the blocks of rows as well as the whole x to All tasks */

  MPI_...
  MPI_...

  for (i=0; i<n; ++i){
    for (j=0; j<ndim; ++j) {
      yloc[i] = yloc[i] + Aloc[i*ndim+j]*x[j];
    }
  }

/*  Combine the full y and make it available to all tasks */

  MPI_...

  // check if sum of Y is the same
  sumy=0;
  if (my_id == root) for (i=0; i<ndim; ++i) {
	  printf("%f ", y[i]);
  }
  
  if (my_id == root) free(A); 
  free(Aloc); 
  free(yloc); 
  free(y); 
  free(x); 
    
  MPI_Finalize();

}



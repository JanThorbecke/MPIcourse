#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

#define root 0
#define n 512

int main(int argc, char *argv[]){
  float *A, *Aloc;
  float *x, *y, *yloc, asum;
  int rc, ntasks, my_id;
  int ndim, i, j, low, up, provided;

  MPI_Init_thread(&argc,&argv,MPI_THREAD_MULTIPLE,&provided);
  MPI_Comm_size(MPI_COMM_WORLD,&ntasks);
  MPI_Comm_rank(MPI_COMM_WORLD,&my_id);
  if (provided!=MPI_THREAD_MULTIPLE && my_id==0) {
	  fprintf(stderr,"MPI_Init_thread asked for MPI_THREAD_MULTIPLE but is not provided.");
  }

  ndim = n*ntasks;
  x=(float *)malloc(ndim*sizeof(float));
  y=(float *)malloc(ndim*sizeof(float));
  yloc=(float *)malloc(n*sizeof(float));
  Aloc= (float *)malloc(n*ndim*sizeof(float));
  if (my_id == root) {
    A= (float *)malloc(ndim*ndim*sizeof(float));
    for (i=0; i<ndim; i++){
      for (j=0; j<ndim; ++j)
        A[i*ndim+j] = (float)drand48();
    }
    x[i] = (float)drand48();
  } 

/*  Transfer the blocks of rows as well as the whole x to  */
/*  tasks */

  MPI_Scatter(A, n*ndim, MPI_FLOAT, Aloc, n*ndim, MPI_FLOAT, 0,MPI_COMM_WORLD);
  MPI_Bcast(x, ndim, MPI_FLOAT, root, MPI_COMM_WORLD);

#pragma omp parallel private(i,j) shared(x,Aloc,ndim) 
  for (i=0; i<n; ++i){
    asum = 0.0;
#pragma omp for reduction(+:asum) 
    for (j=0; j<ndim; ++j) {
      asum = asum + Aloc[i*ndim+j]*x[j];
    }
    yloc[i] = asum;
  }

/*  Combine the full y and make it available to all tasks */

  MPI_Allgather(yloc, n, MPI_FLOAT, y, n, MPI_FLOAT, MPI_COMM_WORLD);

//  if (my_id == root) for (i=0; i<ndim; ++i) printf("%f ", y[i]);
  
  if (my_id == root) free(A); 
  free(Aloc); 
  free(yloc); 
  free(y); 
  free(x); 
    
  MPI_Finalize();

}



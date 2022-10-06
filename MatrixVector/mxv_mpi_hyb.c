#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>
#include <math.h>
#ifdef _OPENMP
#include <omp.h>
#endif

#define root 0
#define n 2048

int main(int argc, char *argv[]){
  float *A, *Aloc;
  float *x, *y, *yloc, asum, ysum;
  int rc, ntasks, my_id, nblock, nthreads;
  int ndim, nn, i, j, low, up, provided;
  double t0, t1;

  MPI_Init_thread(&argc,&argv,MPI_THREAD_MULTIPLE,&provided);
  MPI_Comm_size(MPI_COMM_WORLD,&ntasks);
  MPI_Comm_rank(MPI_COMM_WORLD,&my_id);
  if (provided!=MPI_THREAD_MULTIPLE && my_id==root) {
    fprintf(stderr,"MPI_Init_thread asked for MPI_THREAD_MULTIPLE but is not provided.");
  }
#ifdef _OPENMP
  nthreads = omp_get_max_threads();
#else
  nthreads = 1;
#endif

  ndim = 32768; 
  nn=ceil(ndim/ntasks);
  ndim = nn*ntasks;
  nblock = nn*ndim;
  x=(float *)malloc(ndim*sizeof(float));
  y=(float *)malloc(ndim*sizeof(float));
  yloc=(float *)calloc(nn,sizeof(float));
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

  t0 = MPI_Wtime();
  MPI_Scatter(A, nblock, MPI_FLOAT, Aloc, nblock, MPI_FLOAT, root, MPI_COMM_WORLD);
  MPI_Bcast(x, ndim, MPI_FLOAT, root, MPI_COMM_WORLD);

#pragma omp parallel private(i,j) shared(x,Aloc,ndim,nn) 
  for (i=0; i<nn; ++i){
    asum = 0.0;
#pragma omp for reduction(+:asum) 
    for (j=0; j<ndim; ++j) {
      asum = asum + Aloc[i*ndim+j]*x[j];
    }
    yloc[i] = asum;
  }

/*  Combine the full y and make it available to all tasks */

  MPI_Allgather(yloc, nn, MPI_FLOAT, y, nn, MPI_FLOAT, MPI_COMM_WORLD);

  MPI_Barrier(MPI_COMM_WORLD);
  t1 = MPI_Wtime();
  if (my_id == root){
    fprintf(stderr,"runtime size %d with %d MPI-ranks and %d OMP is %f.3 seconds\n", nn, ntasks, nthreads, t1-t0);
  }

  // check if sum of Y is the same
  ysum=0;
  if (my_id == root) {
    for (i=0; i<ndim; ++i) {
      ysum =+ y[i];
    }
    printf("ysum = %f ", ysum);
  }
  if (my_id == root) free(A); 
  free(Aloc); 
  free(yloc); 
  free(y); 
  free(x); 
    
  MPI_Finalize();

}



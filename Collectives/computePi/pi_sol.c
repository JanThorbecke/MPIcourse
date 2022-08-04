#include <mpi.h>
#include <math.h>
#define root_id 0

int main(int argc, char *argv[]){
  int rc, ntasks, my_id;
  MPI_Init(&argc,&argv);
  MPI_Comm_size(MPI_COMM_WORLD,&ntasks);
  MPI_Comm_rank(MPI_COMM_WORLD,&my_id);

  int n, i;
  double mypi, pi, h, sum, x;
  double t, t0;
  if(my_id==root_id){
    if(argc<2){
      printf("Usage: pi NumberOfIntervals \n");
      exit(1);
    }else{
      n=atoi(argv[1]);
      if(n<1) exit(1);
    }
  }
  t0 = MPI_Wtime();   
  /* Broadcast n to other processes here */
  rc=MPI_Bcast(&n,1,MPI_INT,root_id,MPI_COMM_WORLD);
  /* The function to integrate: f(x)=4/(1+x^2)
     Divide the integration into #tasks slices */
  h=1.0/(double)n;
  sum=0.0;
  for(i=my_id+1;i<=n;i+=ntasks){
    x=h*((double)i-0.5);
    sum+=(4.0/(1.0+x*x));
  }
  mypi=h*sum;
  /* Collect all partial sums to the root process here */
  MPI_Reduce(&mypi,&pi,1,MPI_DOUBLE,MPI_SUM,root_id,
	     MPI_COMM_WORLD);
  t=MPI_Wtime()-t0;
  /* Root node prints the results */
  if(my_id==root_id){
    printf("Obtained approximation for pi is %8.12lf\n", pi);
    printf("Pi to 25 decimal places is 3.141592653589793238462643\n");
    printf("Evaluation took %f seconds with %d tasks.\n", t, ntasks);
  }
  MPI_Finalize();
  return(0);
}


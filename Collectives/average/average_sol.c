#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

int main(int argc, char *argv[]){
  int rc, ntasks, my_id;
  MPI_Init(&argc,&argv);
  MPI_Comm_size(MPI_COMM_WORLD,&ntasks);
  MPI_Comm_rank(MPI_COMM_WORLD,&my_id);
  int data_size;
  if(my_id==0){
    if(argc<1){
      printf("Usage: average numberofelements \n");
      exit(1);
    }else{
      data_size=atoi(argv[1]);
      if(data_size<1) exit(1);
    }
  }
  /* broadcast the number of datapoints to all tasks here */
  rc=MPI_Bcast(&data_size,1,MPI_INT,0,MPI_COMM_WORLD);
  /* Calculate the local amounts, some tasks are going to get
     one more point than the others */
  int local_nr, ival_mod, my_num;
  local_nr = data_size/ntasks;
  ival_mod = data_size%ntasks;
  if(my_id<ival_mod){
    my_num = local_nr + 1;
  }else{
    my_num = local_nr;
  }
  float *aloc;
  aloc=(float *)malloc(my_num*sizeof(aloc));
  printf("I am process %d and I have %d data points.\n",my_id,my_num);
  /* Calculate the data portions! to deliver and generate the data set
     on the root node */
  int *send_counts, *displs;
  float *a;
  send_counts=(int *)malloc(ntasks*sizeof(send_counts));
  displs=(int *)malloc(ntasks*sizeof(displs));
  a=(float *)malloc(data_size*sizeof(a));
  if(my_id==0){
    int i;
    for(i=0;i<ival_mod-1;i++) send_counts[i]=local_nr+1;
    for(i=ival_mod;i<ntasks;i++) send_counts[i]=local_nr;
    displs[0]=0;
    for(i=0;i<ntasks-1;i++) displs[i+1]=displs[i]+send_counts[i];
    for(i=0;i<data_size;i++) a[i]=(float)drand48();
  }
  /* Deliver the number blocks to tasks here */
  rc=MPI_Scatterv(a,send_counts,displs,MPI_FLOAT,
		  aloc,my_num,MPI_FLOAT,0,MPI_COMM_WORLD);
  float psum=0.0;
  int j;
  for (j=0;j<my_num;j++) psum=psum+aloc[j];
  float total_sum;
  /* Gather the local sums into a global sum here */
  rc=MPI_Reduce(&psum,&total_sum,1,MPI_FLOAT,MPI_SUM,
		0,MPI_COMM_WORLD);
  /* the root node prints the results */
  if (my_id==0) {
    printf("Average is %f\n", total_sum/(float)data_size);
  }
  rc=MPI_Finalize();
  return(0);
}

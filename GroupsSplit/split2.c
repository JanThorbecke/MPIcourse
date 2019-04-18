#include <stdio.h>
#include "mpi.h"

int main(int argc, char **argv)
{

  int rank[2], size[2], namelen, color;
  char processor_name[MPI_MAX_PROCESSOR_NAME];
  char *cname[] = { "BLACK", "WHITE", "BLUE" };
  MPI_Comm comm_work;

  MPI_Init(&argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank[0]);
  MPI_Comm_size(MPI_COMM_WORLD, &size[0]);
  MPI_Get_processor_name(processor_name, &namelen);

  fprintf(stderr,"Hello world! I'm  rank %d of %d on %s\n", rank[0], size[0], processor_name);

  color = rank[0]%3;
  MPI_Comm_split(MPI_COMM_WORLD, color, rank[0], &comm_work);

  MPI_Comm_rank(comm_work, &rank[1]);
  MPI_Comm_size(comm_work, &size[1]);

  fprintf(stderr,"%d: I'm  rank %d of %d in the %s context\n", rank[0], rank[1], size[1], cname[color]);


  MPI_Comm_free(&comm_work);
  MPI_Finalize();
}


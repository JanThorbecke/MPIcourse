
Use the minimal MPI program which prints "hello world" by each MPI process.
+ Compile and run it on a single processor.
  Load the following modules on DelftBlue
  module load 2022r2 gcc slurm
  module load openmpi/mlnx/gcc/64/4.1.2rc2
  module load gcc

+ On DelftBlue, or another machine that uses SLURM a workload manager you can use the slurm_solution.job to submit to the queue:
sbatch slurm_solution.job

+ Run it on several processors in parallel.

+ Modify your program such that:
	-1- every process writes its rank and the size of MPI_COMM_WORLD,
	-2- only process ranked 0 in MPI_COMM_WORLD prints "hello world"

+ Why is the sequence of the output non-deterministic?

Discuss with your neighbor what can be done to get the output of all MPI processes on the terminal window in the sequence of the ranks.  Or is there no way to guarantee this?


BACKGROUND INFORMATION

You will notice that the first step to building an MPI program is including the MPI header files with `#include <mpi.h>`. After this, the MPI environment must be initialized with:

MPI_Init( int* argc, char*** argv)

During `MPI_Init`, all of MPI's global and internal variables are constructed. For example, a communicator is formed around all of the processes that were spawned, and unique ranks are assigned to each process. Currently, `MPI_Init` takes two arguments that are not necessary, and the extra parameters are simply left asextra space in case future implementations might need them.

After `MPI_Init`, there are two main functions that are called. These two functions are used in almost every single MPI program that you will write.

MPI_Comm_size(MPI_Comm communicator, int* size)

   MPI_Comm_size(MPI_COMM_WORLD, &size);

`MPI_Comm_size` returns the size of a communicator. In our example, `MPI_COMM_WORLD` (which is constructed for us by MPI) encloses all of the processes in the job, so this call should return the amount of processes that were requested for the job.

MPI_Comm_rank(MPI_Comm communicator, int* rank)

   MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

`MPI_Comm_rank` returns the rank of a process in a communicator. Each process inside of a communicator is assigned an incremental rank starting from zero. The ranks of the processes are primarily used for identification purposes when sending and receiving messages.

MPI_Finalize()

`MPI_Finalize` is used to clean up the MPI environment. No more MPI calls can be made after this one. 

To run a MPI program you can use srun:

srun -np #number_of_tasks --mem-per-cpu=1G ./a.out

srun -n2 --mem-per-cpu=1G ./version_test



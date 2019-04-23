#!/bin/bash
#
#SBATCH -J MatrixVector
#SBATCH --ntasks-per-node=4
#SBATCH --cpus-per-task=4
#SBATCH --nodes=1
#SBATCH --ntasks=4
#SBATCH --time=0:01:00

set -x
echo $SLURM_JOB_NODELIST
cd $SLURM_SUBMIT_DIR

#mpirun -np 2  ./a.out > inter_node.dat

ls
export OMP_NUM_THREADS=1
time srun --mpi=pmi2 --cpu_bind=verbose,core -c $OMP_NUM_THREADS -n 4 ./a.out 

export OMP_NUM_THREADS=4
time srun --mpi=pmi2 --cpu_bind=verbose,core -c $OMP_NUM_THREADS -n 4 ./a.out 

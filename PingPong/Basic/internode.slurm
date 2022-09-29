#!/bin/bash
#
#SBATCH -J PingPing-Internode
#SBATCH --ntasks-per-node=1
#SBATCH --nodes=2
#SBATCH --ntasks=2
#SBATCH --time=0:01:00
#SBATCH --cpus-per-task=1
#SBATCH -p compute
#SBATCH --hint=nomultithread
#SBATCH --mem-per-cpu=1G

set -x
echo $SLURM_JOB_NODELIST
cd $SLURM_SUBMIT_DIR

#mpirun -np 2  ./a.out > inter_node.dat

export OMP_NUM_THREADS=1
srun --mpi=pmix --cpu_bind=verbose,core -c $OMP_NUM_THREADS -n 2 ./a.out > inter_node.dat


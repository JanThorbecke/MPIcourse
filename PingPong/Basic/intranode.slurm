#!/bin/bash
#
#SBATCH -J PingPing-Intranode
#SBATCH --ntasks-per-node=2
#SBATCH --nodes=1
#SBATCH --ntasks=2
#SBATCH --cpus-per-task=1
#SBATCH -p compute
#SBATCH --hint=nomultithread
#SBATCH --mem-per-cpu=1G
#SBATCH --time=0:01:00

echo $SLURM_JOB_NODELIST
cd $SLURM_SUBMIT_DIR

export OMP_NUM_THREADS=1
srun --mpi=pmix --cpu_bind=verbose,core -c $OMP_NUM_THREADS -n 2 ./a.out > intra_node.dat


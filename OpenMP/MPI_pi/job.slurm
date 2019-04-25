#!/bin/bash
#SBATCH -J Name-of-job
#SBATCH --cpus-per-task=1
#SBATCH --ntasks=8
#SBATCH --nodes=2
#SBATCH -o MPI_pi
#SBATCH --time=1:00:00

cd $SLURM_SUBMIT_DIR
mpirun -np $SLURM_NTASKS ./mpi_pi



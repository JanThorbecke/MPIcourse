#!/bin/bash
#
#SBATCH -J Broadcast
# SBATCH --ntasks-per-node=20
#SBATCH --nodes=2
#SBATCH --ntasks=40
#SBATCH --time=0:01:00

set -x
echo $SLURM_JOB_NODELIST
cd $SLURM_SUBMIT_DIR

mpirun -np 40  ./my_bcast
mpirun -np 40  ./compare_bcast 1000 6


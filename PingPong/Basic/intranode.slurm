#!/bin/bash
#
#SBATCH -J PingPing-Intranode
#SBATCH --ntasks-per-node=2
#SBATCH --nodes=1
#SBATCH --ntasks=2
#SBATCH --time=0:01:00

echo $SLURM_JOB_NODELIST
cd $SLURM_SUBMIT_DIR

mpirun -np 2  ./a.out > intra_node.dat


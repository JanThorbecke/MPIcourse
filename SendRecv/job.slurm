#!/bin/bash
#SBATCH -J sendrecv
#SBATCH --cpus-per-task=1
#SBATCH --ntasks=4
#SBATCH --nodes=1
#SBATCH --exclusive
#SBATCH -p compute
#SBATCH --hint=nomultithread
#SBATCH --mem-per-cpu=1G
#SBATCH --time=0:00:09

echo $SLURM_JOB_NODELIST

sleep 10 
srun -n $SLURM_NTASKS ./basic



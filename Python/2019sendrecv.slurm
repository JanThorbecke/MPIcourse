#!/bin/bash
#SBATCH -J mpi4py
#SBATCH --ntasks=2
#SBATCH --nodes=1
#SBATCH -p compute
#SBATCH --hint=nomultithread
#SBATCH --mem-per-cpu=1G
#SBATCH --time=0:01:00
module load 2022r2 slurm
module load openmpi
module load python3

srun -n 2 python3 sendrecv.py


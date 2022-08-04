#!/bin/bash
#SBATCH -J mpi4py
#SBATCH --ntasks=4
#SBATCH --nodes=1
#SBATCH -p compute
#SBATCH --hint=nomultithread
#SBATCH --mem-per-cpu=1G
#SBATCH --time=0:01:00
module load 2022r2 slurm
module load openmpi
module load python3
srun -n 4 python3 broadcast.py


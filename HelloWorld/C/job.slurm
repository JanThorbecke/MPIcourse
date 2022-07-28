#!/bin/bash
#SBATCH -J hello-mpi
#SBATCH --cpus-per-task=1
#SBATCH --ntasks=4
#SBATCH --nodes=1
#SBATCH --exclusive
#SBATCH -p compute
#SBATCH --hint=nomultithread
#SBATCH --mem-per-cpu=1G
##SBATCH -l select=1:ncpus=2
##SBATCH -l place=scatter:exclhost
#SBATCH --time=0:01:00

#mpiexec  -np 2 ./hello_world
#export I_MPI_PMI_LIBRARY=/cm/shared/apps/slurm/current/lib64/libpmi2.so

srun --mpi=pmix -n $SLURM_NTASKS ./hello_world



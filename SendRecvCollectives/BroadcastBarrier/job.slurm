#!/bin/bash
#SBATCH -J bcast-mpi
#SBATCH --cpus-per-task=1
#SBATCH --nodes=8
#SBATCH -p compute
#SBATCH --hint=nomultithread
#SBATCH --mem-per-cpu=1G
#SBATCH --time=0:01:00
#SBATCH --account=research-ceg-gse
##SBATCH --account=education-ceg-msc
#SBATCH --ntasks-per-node=1

set -x
echo $SLURM_JOB_NODELIST
cd $SLURM_SUBMIT_DIR

srun -n $SLURM_NTASKS ./compare_bcast 1000 6


#!/bin/bash
#
#SBATCH -J PingPing-Internode
#SBATCH --ntasks-per-node=40
#SBATCH --nodes=1
#SBATCH --exclusive
#SBATCH --ntasks=40
#SBATCH --time=0:01:00
#SBATCH --cpus-per-task=1
#SBATCH -p compute
#SBATCH --hint=nomultithread
#SBATCH --mem-per-cpu=1G

set -x
echo $SLURM_JOB_NODELIST
cd $SLURM_SUBMIT_DIR

ulimit -s unlimited

srun --mpi=pmix --cpu_bind=verbose,core -n $SLURM_NTASKS ping_pong_advanced2_ssend > ssend.dat

srun --mpi=pmix --cpu_bind=verbose,core -n $SLURM_NTASKS ping_pong_advanced2_send > send.dat


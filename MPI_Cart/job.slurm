#!/bin/bash
#SBATCH -J hello-mpi
#SBATCH --cpus-per-task=1
#SBATCH --ntasks=16
#SBATCH --nodes=1
#SBATCH --exclusive
#SBATCH -p compute
#SBATCH --hint=nomultithread
#SBATCH --mem-per-cpu=1G
#SBATCH --time=0:01:00

set -x
cd $SLURM_SUBMIT_DIR

starttime=`date +%s%N`

srun  -n 16 ./ring_2d_sr

endtime=`date +%s%N`

runtime=$(echo "scale=9; 1.0*10^(-9)*(${endtime}-${starttime})" | bc -l)
echo "Runtime = $runtime seconds" 


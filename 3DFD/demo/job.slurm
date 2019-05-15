#!/bin/bash
#SBATCH -n 64
#SBATCH --exclusive
#SBATCH -t 0:05:00
#SBATCH --export=ALL

set -x
cd $SLURM_SUBMIT_DIR

procppn=32
ppn=32
nmpi=64
export OMP_NUM_THREADS=1
exe=../3dfd

starttime=`date +%s%N`

mpirun -np $nmpi $exe verbose=3

endtime=`date +%s%N`
runtime=$(echo "scale=9; 1.0*10^(-9)*(${endtime}-${starttime})" | bc -l)
echo "Runtime = $runtime seconds" 


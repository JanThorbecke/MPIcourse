#!/bin/bash
#
set -x
cd $SLURM_SUBMIT_DIR

starttime=`date +%s%N`

mpirun  -np 16 ./ring_2d_sr

endtime=`date +%s%N`

runtime=$(echo "scale=9; 1.0*10^(-9)*(${endtime}-${starttime})" | bc -l)
echo "Runtime = $runtime seconds" 


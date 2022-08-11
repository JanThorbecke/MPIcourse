#!/bin/bash
#SBATCH -J 3dfd
#SBATCH --cpus-per-task=1
#SBATCH --ntasks=16
#SBATCH --nodes=1
##SBATCH --exclusive
#SBATCH -p compute
#SBATCH --hint=nomultithread
#SBATCH --mem-per-cpu=1G
#SBATCH --time=0:02:00

set -x
cd $SLURM_SUBMIT_DIR

module load papi
#export LD_PRELOAD=/home/jthorbecke/tools/mpitrace/src/libmpitrace.so
export LD_PRELOAD=/home/jthorbecke/tools/mpitrace/src/libmpihpm.so
export SAVE_LIST=0,4,8,12,15
export TRACE_SEND_PATTERN=yes
#export PROFILE_BY_COMMUNICATOR=yes
export ENABLE_TRACING=yes
export TRACE_ALL_TASKS=yes

starttime=`date +%s%N`

srun  -n 16 ./3dfd nt=1000 itsnap=2000

endtime=`date +%s%N`

runtime=$(echo "scale=9; 1.0*10^(-9)*(${endtime}-${starttime})" | bc -l)
echo "Runtime = $runtime seconds" 


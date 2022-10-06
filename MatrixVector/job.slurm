#!/bin/bash
#SBATCH -J MatrixVector
#SBATCH -p compute
#SBATCH --hint=nomultithread
#SBATCH --mem-per-cpu=1G
#SBATCH --time=0:15:00
#SBATCH --account=research-ceg-gse
##SBATCH --account=education-ceg-msc
##SBATCH --ntasks-per-node=4
#SBATCH --cpus-per-task=4
#SBATCH --nodes=1
#SBATCH --ntasks=8
#SBATCH --exclusive

set -x
echo $SLURM_JOB_NODELIST
cd $SLURM_SUBMIT_DIR

export OMP_PLACES=cores
export OMP_PROC_BIND=true
export OMP_DISPLAY_AFFINITY=true
export OMP_AFFINITY_FORMAT="Thread Affinity: Process %P thread %.4n affinity %.4{thread_affinity} on node %.8H"

export OMP_NUM_THREADS=1
time srun --cpu_bind=verbose,core -c $OMP_NUM_THREADS -n $SLURM_NTASKS ./a.out 

export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK
time srun --cpu_bind=verbose,core -c $OMP_NUM_THREADS -n $SLURM_NTASKS ./a.out 

exit
#export OMP_PROC_BIND={true,close,spread}
time srun --cpu_bind=verbose,map_cpu:0,4,8,12 -c $OMP_NUM_THREADS -n 4 ./a.out 
time srun --cpu_bind=verbose,mask_cpu:0xF,0xF000,0xF000000,0xF000000000 -c $OMP_NUM_THREADS -n 4 ../Affinity/xthi
time srun --cpu_bind=verbose,mask_cpu:0xF,0xF000,0xF000000,0xF000000000 -c $OMP_NUM_THREADS -n 4 ./a.out


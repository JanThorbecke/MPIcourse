#!/bin/bash 
#SBATCH -J affinity-mpi
#SBATCH --ntasks=12
#SBATCH --nodes=1
#SBATCH --exclusive
#SBATCH -p compute
#SBATCH --hint=nomultithread
#SBATCH --mem-per-cpu=1G
#SBATCH --time=0:01:00
#SBATCH --account=research-ceg-gse

# Note the grid_order script is part of module perftools-base

export OMP_NUM_THREADS=1
mapcpu="map_cpu:0,1,2,5,6,8,9,10,12,13,14,18,20,21,22,24"
mapcpu="map_cpu:0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45"

set -x
srun -n1 more /proc/cpuinfo  | grep "cpu cores" | uniq
srun -n 16 -m plane=16 --cpu-bind=verbose,$mapcpu ./xthi | sort -nk4
srun -n $SLURM_NTASKS --cpu-bind=verbose,$mapcpu ./xthi | sort -nk4

(( socket = SLURM_NTASKS/2 ))
srun -n $SLURM_NTASKS --ntasks-per-socket=$socket --cpu-bind=verbose,$mapcpu ./xthi | sort -nk4

export OMP_NUM_THREADS=4
srun -n $SLURM_NTASKS --cpu-bind=rank --cpus-per-task=$OMP_NUM_THREADS ./xthi | sort -nk4

export OMP_PLACES=cores
export OMP_PROC_BIND=true
export OMP_DISPLAY_AFFINITY=true
time srun --cpu_bind=verbose,mask_cpu:0xF,0xF0,0xF00,0xF000 -c $OMP_NUM_THREADS -n 4 ./xthi
time srun --cpu_bind=verbose,mask_cpu:0xF,0xF000,0xF000000,0xF000000000 -c $OMP_NUM_THREADS -n 4 ./xthi

#mpirun -np 16 --report-bindings ./xthi | sort -nk4
#echo "*mpirun spu-list *"
#mpirun -np 16 --cpu-list "0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45" --report-bindings ./xthi | sort -nk4


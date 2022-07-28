#!/bin/bash
#SBATCH -J deadlock
#SBATCH --cpus-per-task=1
#SBATCH --ntasks=2
#SBATCH --nodes=2
#SBATCH -p compute
#SBATCH --hint=nomultithread
#SBATCH --mem-per-cpu=1G
##SBATCH --exclusive
##SBATCH -l select=1:ncpus=2
##SBATCH -l place=scatter:exclhost
#SBATCH --time=0:01:00

#ompi_info --param btl all --level 9
export OMPI_MCA_btl_self_rndv_eager_limit=256
export OMPI_MCA_btl_self_eager_limit=256
export OMPI_MCA_btl_sm_eager_limit=256
export OMPI_MCA_btl_vader_eager_limit=256
export OMPI_MCA_btl_sm_max_send_size=256
export OMPI_MCA_osc_rdma_buffer_size=256
#export OMPI_MCA_mpi_show_mca_params=all

export SLURM_CPU_BIND=verbose
#echo $SLURM_JOB_NUM_NODES
#squeue -j -l $SLURM_JOBID | grep $SLURM_JOBID

srun --mpi=pmix -N2 --ntasks-per-node=1 -n $SLURM_NTASKS ./deadlock

#mpirun -n $SLURM_NTASKS ./deadlock



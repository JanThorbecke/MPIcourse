#!/bin/bash
#SBATCH -J hello-mpi
#SBATCH --cpus-per-task=1
#SBATCH --ntasks=2
#SBATCH --nodes=1
#SBATCH --exclusive
#SBATCH -p compute
#SBATCH --hint=nomultithread
#SBATCH --mem-per-cpu=1G
#SBATCH -o helloF90-%A.out
##SBATCH -l select=1:ncpus=2
##SBATCH -l place=scatter:exclhost
#SBATCH --time=0:01:00

#mpiexec  -np 2 ./hello_world
#export I_MPI_PMI_LIBRARY=/cm/shared/apps/slurm/current/lib64/libpmi2.so

export OMPI_MCA_btl_sm_eager_limit=8192
export OMPI_MCA_btl_vader_eager_limit=8192
export OMPI_MCA_mpi_show_mca_params=all

#mpirun -np 2 --mca btl_base_verbose 100 ./hello
mpirun --verbose -np $SLURM_NTASKS ./hello

module load likwid
srun --mpi=pmix -n2 likwid-perfctr -C 0-3 -g CLOCK ./hello_world



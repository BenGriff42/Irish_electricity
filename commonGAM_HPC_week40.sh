#!/bin/bash

#SBATCH --job-name=commonGAM_test_week40
#SBATCH --partition=compute
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=8
#SBATCH --time=24:00:00
#SBATCH --mem=190G

cd "${SLURM_SUBMIT_DIR}"

echo Running on host "$(hostname)"
echo Time is "$(date)"
echo Directory is "$(pwd)"
echo Slurm job ID is "${SLURM_JOBID}"
echo This jobs runs on the following machines:
echo "${SLURM_JOB_NODELIST}"

module add lang/r/4.1.2-gcc
module load lang/r/4.1.2-gcc

export OMP_NUM_THREADS=1

Rscript commonGAM_HPC_week40.R


#!/bin/sh

#PBS -l walltime=08:00:00
#PBS -l select=1:ncpus=4:ompthreads=1:mem=50gb

REPO_PATH=/rds/general/user/sd121/home/bayes-rate-consistency

module load anaconda3/personal
source activate bayes-rate-consistency

cd $REPO_PATH

# Run postprocessing script
Rscript scripts/sim-postprocess.R

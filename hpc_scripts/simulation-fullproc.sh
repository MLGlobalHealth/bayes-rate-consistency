#!/bin/sh

#PBS -l walltime=08:00:00
#PBS -l select=1:ncpus=4:ompthreads=1:mem=50gb
#PBS -J 1-2

REPO_PATH=/rds/general/user/sd121/home/bayes-rate-consistency

module load anaconda3/personal
source activate bayes-rate-consistency

cd $REPO_PATH

# Generate simulated datasets
Rscript scripts/sim-dataset.R --idx $PBS_ARRAY_INDEX

# Run Stan models
Rscript scripts/sim-run-stan.R --idx $PBS_ARRAY_INDEX

# Postprocessing
Rscript scripts/sim-postprocess.R --idx $PBS_ARRAY_INDEX

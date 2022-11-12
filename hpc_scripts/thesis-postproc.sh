#!/bin/sh

#PBS -l walltime=01:00:00
#PBS -l select=1:ncpus=10:ompthreads=1:mem=50gb

REPO_PATH=/rds/general/user/sd121/home/covimod-gp
MODEL="lrd-hsgp"
WAVES=5

module load anaconda3/personal
source activate Renv

MODEL=${MODEL}-${WAVES}

Rscript $REPO_PATH/scripts/thesis-postproc.R --model $MODEL

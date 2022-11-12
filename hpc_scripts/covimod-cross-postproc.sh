#!/bin/sh

#PBS -l walltime=02:00:00
#PBS -l select=1:ncpus=8:ompthreads=1:mem=50gb

REPO_PATH=/rds/general/user/sd121/home/covimod-gp
WAVE=4
MODEL="rd-hsgp"

# Post-processing
MIXING=TRUE
PPC=TRUE
PLOT=TRUE

module load anaconda3/personal
source activate Renv

MODEL=${MODEL}-${WAVE}
Rscript $REPO_PATH/scripts/postprocess-single.R --model $MODEL --mixing $MIXING --ppc $PPC --plot $PLOT

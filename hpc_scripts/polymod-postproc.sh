
#!/bin/sh

#PBS -l walltime=08:00:00
#PBS -l select=1:ncpus=4:ompthreads=1:mem=100gb

REPO_PATH=/rds/general/user/sd121/home/covimod-gp

SEED=1527
MODEL="hsgp-m52-rd"
HSGP_C=1.5
HSGP_M1=20
HSGP_M2=20

MIXING=FALSE
PPC=TRUE
PLOT=TRUE

module load anaconda3/personal
source activate Renv

# Postprocessing
Rscript $REPO_PATH/scripts/polymod-postprocess.R --model $MODEL --mixing $MIXING --ppc $PPC --plot $PLOT

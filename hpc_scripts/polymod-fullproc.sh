
#!/bin/sh

#PBS -l walltime=08:00:00
#PBS -l select=1:ncpus=10:ompthreads=1:mem=100gb

REPO_PATH=/rds/general/user/sd121/home/covimod-gp

SEED=1527
MODEL="hsgp-m52-rd"
HSGP_C=1.5
HSGP_M1=40
HSGP_M2=20

MIXING=FALSE
PPC=TRUE
PLOT=TRUE

module load anaconda3/personal
source activate Renv

# Run Stan models
Rscript $REPO_PATH/scripts/polymod-run-stan.R --model $MODEL --hsgp_c $HSGP_C --hsgp_m1 $HSGP_M1 --hsgp_m2 $HSGP_M2

# Postprocessing
Rscript $REPO_PATH/scripts/polymod-postprocess.R --model $MODEL --mixing $MIXING --ppc $PPC --plot $PLOT

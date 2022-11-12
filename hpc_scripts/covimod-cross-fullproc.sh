#!/bin/sh

#PBS -l walltime=08:00:00
#PBS -l select=1:ncpus=10:ompthreads=1:mem=100gb

REPO_PATH=/rds/general/user/sd121/home/covimod-gp
WAVE=5
MODEL="rd-hsgp"
HSGP_C=1.5
HSGP_M=20

# HMC Sampler params
CHAINS=8
WARMUP=500
SAMPLING=1000

# Post-processing
MIXING=TRUE

module load anaconda3/personal
source activate Renv

Rscript $REPO_PATH/scripts/run-stan-single.R --wave $WAVE --model $MODEL --hsgp_c $HSGP_C --hsgp_m $HSGP_M --chains $CHAINS --iter_warmup $WARMUP --iter_sampling $SAMPLING

MODEL=${MODEL}-${WAVE}
Rscript $REPO_PATH/scripts/postprocess-single.R --model $MODEL --mixing $MIXING

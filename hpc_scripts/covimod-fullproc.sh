#!/bin/sh

#PBS -l walltime=48:00:00
#PBS -l select=1:ncpus=10:ompthreads=1:mem=512gb

REPO_PATH=/rds/general/user/sd121/home/covimod-gp
WAVES=5
MODEL="hsgp-m52-lrd"
HSGP_C=1.5
HSGP_M1=40
HSGP_M2=30

# HMC Sampler params
CHAINS=8
WARMUP=500
SAMPLING=1000

module load anaconda3/personal
source activate Renv

Rscript $REPO_PATH/scripts/run-stan.R --waves $WAVES --model $MODEL --hsgp_c $HSGP_C --hsgp_m1 $HSGP_M1 --hsgp_m2 $HSGP_M2 --chains $CHAINS --iter_warmup $WARMUP --iter_sampling $SAMPLING

MODEL=${MODEL}-${WAVES}
Rscript $REPO_PATH/scripts/postprocess.R --model $MODEL

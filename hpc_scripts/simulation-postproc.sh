d#!/bin/sh

#PBS -l walltime=08:00:00
#PBS -l select=1:ncpus=4:ompthreads=1:mem=50gb
#PBS -J 1-10

REPO_PATH=/rds/general/user/sd121/home/covimod-gp

SEED=1527
COVID="FALSE"
N=2000
STRATA="COVIMOD"

MODEL="hsgp-m52-rd"
HSGP_M1=30
HSGP_M2=20

if [ $COVID = "TRUE" ]
then
  DATA=inCOVID_${N}_${STRATA}
else
  DATA=preCOVID_${N}_${STRATA}
fi

if [[ $MODEL = *"hsgp"* ]]
then
  MODEL=${MODEL}-${HSGP_M1}-${HSGP_M2}
fi

module load anaconda3/personal
source activate Renv

Rscript $REPO_PATH/scripts/sim-postprocess.R --model $MODEL --data $DATA --idx $PBS_ARRAY_INDEX

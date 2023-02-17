
#!/bin/sh

#PBS -l walltime=08:00:00
#PBS -l select=1:ncpus=4:ompthreads=1:mem=50gb
#PBS -J 1-10

REPO_PATH=/rds/general/user/sd121/home/covimod-gp

SEED=1527
COVID="TRUE"
N=2000
STRATA="COVIMOD"

MODEL="hsgp-m32-rd"
HSGP_C=1.5
HSGP_M1=30
HSGP_M2=20

if [ $COVID = "TRUE" ]
then
  DATA=inCOVID_${N}_${STRATA}
else
  DATA=preCOVID_${N}_${STRATA}
fi

module load anaconda3/personal
source activate Renv

# Generate simulated datasets
Rscript $REPO_PATH/scripts/sim-dataset.R --seed $SEED --covid $COVID --size $N --strata $STRATA

# Run Stan models
Rscript $REPO_PATH/scripts/sim-run-stan.R --seed $SEED --model $MODEL --data $DATA --hsgp_binf $HSGP_C --hsgp_m1 $HSGP_M1 --hsgp_m2 $HSGP_M2 --idx $PBS_ARRAY_INDEX

if [[ $MODEL = *"hsgp"* ]]
then
  MODEL=${MODEL}-${HSGP_M1}-${HSGP_M2}
fi

# Postprocessing
Rscript $REPO_PATH/scripts/sim-postprocess.R --model $MODEL --data $DATA --idx $PBS_ARRAY_INDEX

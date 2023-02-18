#!/bin/bash

REPO_PATH=/rds/general/user/sd121/home/bayes-rate-consistency
OUT_PATH=/rds/general/user/sd121/home/bayes-rate-consistency-output

cat > "$OUT_PATH/simulation-postproc-test.pbs" <<EOF
#PBS -l walltime=08:00:00
#PBS -l select=1:ncpus=4:ompthreads=1:mem=50gb

module load anaconda3/personal
source activate bayes-rate-consistency

cd $REPO_PATH

# Run postprocessing script
Rscript scripts/sim-postprocess.R -i "$REPO_PATH" -o "$OUT_PATH"
EOF

cd $OUT_PATH
qsub simulation-postproc-test.pbs

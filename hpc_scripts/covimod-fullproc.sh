#!/bin/bash
REPO_PATH="/rds/general/user/sd121/home/bayes-rate-consistency"
OUT_PATH="/rds/general/user/sd121/home/bayes-rate-consistency-output"
CONFIG_FILE="covimod-longitudinal.yml"

# Create main script
cat > "$OUT_PATH/covimod-fullproc.pbs" <<EOF
#!/bin/bash
#PBS -l walltime=48:00:00
#PBS -l select=1:ncpus=9:ompthreads=1:mem=250gb

module load anaconda3/personal
source activate bayes-rate-consistency

# Move into repository
cd $REPO_PATH

# Run Stan models
Rscript scripts/run-stan.R \
  -i "$REPO_PATH" \
  -o "$OUT_PATH" \
  --config "$CONFIG_FILE"

# Postprocessing
Rscript scripts/postprocess.R \
  -i "$REPO_PATH" \
  -o "$OUT_PATH" \
  --config "$CONFIG_FILE"
EOF

# Execute main script
cd $OUT_PATH
qsub "covimod-fullproc.pbs"

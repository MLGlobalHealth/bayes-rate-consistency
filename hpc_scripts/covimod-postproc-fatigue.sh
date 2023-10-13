#!/bin/bash
REPO_PATH="/rds/general/user/sd121/home/bayes-rate-consistency"
OUT_PATH="/rds/general/user/sd121/home/bayes-rate-consistency-output"
CONFIG_FILE="fatigue-interaction.yml"

# Create main script
cat > "$OUT_PATH/covimod-postproc-fatigue.pbs" <<EOF
#!/bin/bash
#PBS -l walltime=08:00:00
#PBS -l select=1:ncpus=8:ompthreads=1:mem=256gb

module load anaconda3/personal
source activate bayes-rate-consistency

# Move into repository
cd $REPO_PATH

# Postprocessing
Rscript scripts/postprocess-fatigue-models.R \
  -i "$REPO_PATH" \
  -o "$OUT_PATH" \
  --config "$CONFIG_FILE"
EOF

# Execute main script
cd $OUT_PATH
qsub "covimod-postproc-fatigue.pbs"
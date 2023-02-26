#!/bin/bash
REPO_PATH=/rds/general/user/sd121/home/bayes-rate-consistency
OUT_PATH=/rds/general/user/sd121/home/bayes-rate-consistency-output
CONFIG_FILE=zimbabwe-stratified-age.yml

# Create main script
cat > "$OUT_PATH/cross-sectional-postproc.pbs" <<EOF
#!/bin/bash
#PBS -l walltime=08:00:00
#PBS -l select=1:ncpus=8:ompthreads=1:mem=100gb

module load anaconda3/personal
source activate bayes-rate-consistency

# Move into repository
cd $REPO_PATH

# Postprocessing
Rscript scripts/postprocess-single-wave.R \
  -i "$REPO_PATH" \
  -o "$OUT_PATH" \
  --config "$CONFIG_FILE"
EOF

# Execute main script
cd $OUT_PATH
qsub cross-sectional-postproc.pbs

#!/bin/bash
REPO_PATH=/rds/general/user/sd121/home/bayes-rate-consistency
OUT_PATH=/rds/general/user/sd121/home/bayes-rate-consistency-output
CONFIG_FILE=simulation.yml

# Create main script
cat > "$OUT_PATH/simulation-fullproc.pbs" <<EOF
#!/bin/bash

#PBS -l walltime=08:00:00
#PBS -l select=1:ncpus=4:ompthreads=1:mem=50gb
#PBS -J 1-10

module load anaconda3/personal
source activate bayes-rate-consistency

# Move into repository
cd "$REPO_PATH"

# Make a copy of the configuration file in the output directory
cp "settings/$CONFIG_FILE" "$OUT_PATH"

# Generate simulated datasets
Rscript scripts/sim-dataset.R \
  -i "$REPO_PATH" \
  -o "$OUT_PATH" \
  --config "$OUT_PATH/$CONFIG_FILE" \
  --pidx "\$PBS_ARRAY_INDEX"

# Run Stan models
Rscript scripts/sim-run-stan.R \
  -i "$REPO_PATH" \
  -o "$OUT_PATH" \
  --config "$OUT_PATH/$CONFIG_FILE" \
  --pidx "\$PBS_ARRAY_INDEX"

# Postprocessing
Rscript scripts/sim-postprocess.R \
  -i "$REPO_PATH" \
  -o "$OUT_PATH" \
  --config "$OUT_PATH/$CONFIG_FILE" \
  --pidx "\$PBS_ARRAY_INDEX"
EOF

# Execute main script
cd $OUT_PATH
qsub simulation-fullproc.pbs

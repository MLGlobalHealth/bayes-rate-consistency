#!/bin/bash
REPO_PATH=/rds/general/user/sd121/home/bayes-rate-consistency
OUT_PATH=/rds/general/user/sd121/home/bayes-rate-consistency-output

# Create main script
cat > "$OUT_PATH/sup-figure-11.pbs" <<EOF
#!/bin/bash
#PBS -l walltime=01:00:00
#PBS -l select=1:ncpus=8:ompthreads=1:mem=32gb

module load anaconda3/personal
source activate bayes-rate-consistency

# Move into repository
cd $REPO_PATH

# Make trace plot
Rscript ./paper/figure_scripts/sup-figure-11.R
EOF

# Execute main script
cd $OUT_PATH
qsub "sup-figure-11.pbs"

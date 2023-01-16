# Creates simulated datasets from simulated intensity data
# For detailed documentation on how this works refer to the Simulating contact dataset notebook

cat("\n---------- Simulating contact dataset ----------\n")

# Load libraries
library(yaml)
library(optparse)
library(data.table)
library(ggplot2)
library(ggpubr)

# Read CLI arguments (for batch jobs on the HPC)
option_list <- list(
  make_option("--idx", type="integer", default=0,
              help="PBD_JOB_IDX",
              dest="idx")
)
cmdargs <- parse_args(OptionParser(option_list = option_list))

# Read simulation parameters
params <- yaml::read_yaml("../settings/simulation.yml")

###### ---------- Load data ---------- #####
source(file.path(params$repo_path, "R", "sim-dataset-utility.R"))

# Import simulated intensity
if ( params$data$covid ) {
  params$data$path <- file.path("data/simulations/intensity", "inCOVID")
} else {
  params$data$path <- file.path("data/simulations/intensity", "preCOVID")
}

dt <- as.data.table(readRDS( file.path(params$repo_path, params$data$path, "data.rds") ))        # Contact intensity
dt.pop <- as.data.table(read.csv(file.path(params$repo_path, "data/germany-population-2011.csv"))) # Population count

##### ---------- configure data export ---------- #####
# Create the directory to export the simulated data
dir_name <- paste(ifelse(params$data$covid, "inCOVID", "preCOVID"), params$data$size, params$data$strata, sep="_")
export_path <- file.path(params$out_path, "data/simulations/datasets", dir_name)
if(!dir.exists(export_path)){
  cat(paste("\n Making export directory:", export_path))
  dir.create(export_path, recursive = TRUE)
}

##### ---------- Stratify age groups ---------- #####
cat("\n Stratifying age groups ...")
dt <- stratify_alter_age(dt, params$data$strata)

##### ---------- Simulating contact survey data ---------- ##########
cat("\n Generating contact dataset ...")
N <- params$data$size # Survey sample size

# Proportion of each age in population
dt.pop[, weight := pop/sum(pop)]

# Participant size for each age
dt.pop[, part := round(weight*N)]

# Calculate average contact counts
dt <- merge(dt, dt.pop[, list(age, gender, part)], all.x=T, by=c("age", "gender"))
dt[, mu := round(cntct_intensity*part)]

# Simulate contact counts in survey
set.seed(params$seed + cli_args$idx)

dt[, y := rpois(nrow(dt), lambda=dt$mu)]

# Stratify contact intensities and contact rates
group_var <- c("age", "gender", "alter_age_strata", "alter_gender")
dt[, y_strata := sum(y), by=group_var]
dt[, cntct_intensity_strata := sum(cntct_intensity), by=group_var]
dt[, pop_strata := sum(pop), by=group_var]
dt[, cntct_rate_strata := sum(cntct_intensity)/sum(pop), by=group_var]

# Save data
saveRDS(dt, file = file.path(export_path, paste0("data", "_", cmdargs$idx, ".rds")))

cat("\n DONE. \n")

# Creates simulated datasets from simulated intensity data
# For detailed documentation on how this works refer to the Simulating contact dataset notebook

cat("\n---------- Simulating contact dataset ----------\n")

# Load libraries
library(yaml)
library(optparse)
library(data.table)

# Read CLI arguments (for batch jobs on the HPC)
option_list <- list(
  make_option(c("-i", "--in"), type = "character", default = NA, help = "repository path", dest = "repo_path"),
  make_option(c("-o", "--out"), type = "character", default = NA, help = "output path", dest = "out_path"),
  make_option(c("--config"), type = "character", default = NA, help = "configuration file", dest = "config_file"),
  make_option("--pidx", type = "integer", default = 0,
              help = "PBD_JOB_IDX",
              dest = "pidx")
)
cli_params <- parse_args(OptionParser(option_list = option_list))

# Read simulation parameters
config <- yaml::read_yaml(file.path(getwd(),
                                    "config",
                                    cli_params$config_file))

###### ---------- Load data ---------- #####
# Import simulated intensity data
intensity_path <- ifelse(config$data$covid, "inCOVID", "preCOVID")
intensity_file <- file.path("data/simulations/intensity", intensity_path, "data.rds")
dt <- as.data.table(readRDS(file.path(cli_params$repo_path, intensity_file)))

# Import population count data
pop_file <- file.path(cli_params$repo_path, "data/germany-population-2011.csv")
dt_population <- as.data.table(read.csv(pop_file))

##### ---------- configure data export ---------- #####
# Configure data export settings
export_dir_name <- paste(ifelse(config$data$covid, "inCOVID", "preCOVID"),
                         config$data$size,
                         config$data$strata,
                         sep = "_")
export_path <- file.path(cli_params$out_path, "data/simulations/datasets", export_dir_name)

# Create the directory to export the simulated data, if it doesn't exist
if (!dir.exists(export_path)) {
  cat(paste("\n Making export directory:", export_path))
  dir.create(export_path, recursive = TRUE)
}

##### ---------- Stratify age groups ---------- #####

# Display progress message
cat("\n Stratifying age groups ...")

# Stratify age groups using the `stratify_contact_age()` function
source(file.path(cli_params$repo_path, "R", "stratify_contact_age.R"))
dt <- stratify_contact_age(dt, config$data$strata)

##### ---------- Simulating contact survey data ---------- ##########

# Display progress message
cat("\n Generating contact dataset ...")

# Set the survey sample size
N <- config$data$size

dt_partsize <- merge(unique(dt[,.(age, gender)]), dt_population,
                     all.x = TRUE,
                     by = c("age", "gender"))

# Calculate the proportion of each age in the population
dt_partsize[, weight := pop / sum(pop)]
dt_partsize[, part := round(weight * N)]

# Calculate the average contact counts
dt <- merge(dt, dt_partsize[, list(age, gender, part)],
            all.x = TRUE,
            by = c("age", "gender"))

dt[, mu := round(cntct_intensity * part)]

# Simulate contact counts in the survey
set.seed(config$seed + cli_params$pidx)
dt[, y := rpois(nrow(dt), lambda = dt$mu)]

# Stratify contact intensities and contact rates
group_var <- c("age", "gender", "alter_age_strata", "alter_gender")
dt[, y_strata := sum(y), by = group_var]
dt[, cntct_intensity_strata := sum(cntct_intensity), by = group_var]
dt[, pop_strata := sum(pop), by = group_var]
dt[, cntct_rate_strata := sum(cntct_intensity) / sum(pop), by = group_var]

# Save the data
file_name <- paste0("data", "_", cli_params$pidx, ".rds")
file_path <- file.path(export_path, file_name)
saveRDS(dt, file = file_path)

cat("\n DONE. \n")

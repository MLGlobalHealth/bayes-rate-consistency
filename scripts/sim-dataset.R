# Creates simulated datasets from simulated intensity data
# For detailed documentation on how this works refer to the Simulating contact dataset notebook

cat("\n---------- Simulating contact dataset ----------\n")

# Load libraries
library(optparse)
library(data.table)
library(ggplot2)
library(ggpubr)

option_list <- list(
  optparse::make_option("--seed", type = "integer", default = 0721, dest = "seed"),
  optparse::make_option("--size", type = "integer", default = 2000,
                        help = "Number of participants in the survey [default \"%default\"]",
                        dest = 'size'),
  optparse::make_option("--nsim", type = "integer", default = 10,
                        help = "Number of simulated datasets [default \"%default\"]",
                        dest = "nsim"),
  optparse::make_option("--strata", type = "character", default = "COVIMOD",
                        help = "Age stratification scheme [default %default]",
                        dest = "strata"),
  optparse::make_option("--covid", type = "logical", default = TRUE,
                        help = "Simulate contact patterns in a hypothetical scenario with restricted contact patterns",
                        dest = "covid"),
  optparse::make_option("--repo_path", type = "character", default = "/rds/general/user/sd121/home/covimod-gp",
                        help = "Absolute file path to repository directory [default]",
                        dest = 'repo.path')
)

args <- optparse::parse_args(optparse::OptionParser(option_list = option_list))

# args$repo.path <- "~/Imperial/covimod-gp"
# args$strata <- "5yr"

##### ---------- Error handling ---------- #####
if(is.na(args$repo.path)){
  stop("Please specify --repo_path")
}

###### ---------- Load data ---------- #####
source(file.path(args$repo.path, "R", "sim-dataset-utility.R"))

# Import simulated intensity
if (args$covid) {
  args$data.path <- file.path("data/simulations/intensity", "inCOVID")
} else {
  args$data.path <- file.path("data/simulations/intensity", "preCOVID")
}

dt <- as.data.table(readRDS( file.path(args$repo.path, args$data.path, "data.rds") ))

# Population count data
dpop <- as.data.table(read.csv(file.path(args$repo.path, "data/germany-population-2011.csv")))
dpop <- dpop[age >= 6 & age <= 49]

##### ---------- Data export ---------- #####
# Directory under data/simulations/datasets to export the data
dir.name <- paste(ifelse(args$covid, "inCOVID", "preCOVID"), args$size, args$strata, sep="_")

export.path <- file.path(args$repo.path, "data/simulations/datasets", dir.name)
if(!dir.exists(export.path)){
  cat(paste("\n Making export directory:", export.path))
  dir.create(export.path)
}

##### ---------- Stratify age groups ---------- #####
cat("\n Stratifying age groups ...")
dt <- stratify_alter_age(dt, args$strata)

##### ---------- Simulating contact survey data ---------- ##########
cat("\n Generating contact dataset ...")
N <- args$size # Survey sample size

# Proportion of each age in population
dpop[, weight := pop/sum(pop)]

# Participant size for each age
dpop[, part := round(weight*N)]

# Calculate average contact counts
dt <- merge(dt, dpop[, list(age, gender, part)], by=c("age", "gender"))
dt[, mu := round(cntct_intensity*part)]

# Simulate contact counts in survey
for (i in 1:args$nsim){
  set.seed(args$seed + i)

  dt[, y := rpois(nrow(dt), lambda=dt$mu)]

  # Stratify contact intensities and contact rates
  group_var <- c("age", "gender", "alter_age_strata", "alter_gender")
  dt[, y_strata := sum(y), by=group_var]
  dt[, cntct_intensity_strata := sum(cntct_intensity), by=group_var]
  dt[, pop_strata := sum(pop), by=group_var]
  dt[, cntct_rate_strata := sum(cntct_intensity)/sum(pop), by=group_var]

  # Save data
  saveRDS(dt, file = file.path(export.path, paste0("data","_",i,".rds")))
}

cat("\n DONE. \n")

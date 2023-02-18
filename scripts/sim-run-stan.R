# Run Stan models
cat("\n---------- Compiling and Running Stan Model ----------\n")

# Import libraries
library(optparse)
library(data.table)
library(stringr)
library(cmdstanr)
library(yaml)

##### ---------- I/O ---------- #####

cat(" Loading experiment parameters ...\n")

# Read CLI arguments (for batch jobs on the HPC)
option_list <- list(
  make_option("--idx", type="integer", default=0,
              help="PBD_JOB_IDX",
              dest="idx")
)
cli_params <- parse_args(OptionParser(option_list = option_list))
experiment_params <- read_yaml(file.path(getwd(), "settings/simulation.yml"))

##### ---------- Configuring Stan data ---------- #####

cat(" Loading simulated data ...\n")

data_params <- experiment_params$data
dataset_name <- paste(ifelse(data_params$covid, "inCOVID", "preCOVID"),
                      data_params$size,
                      data_params$strata,
                      sep = '_')
data_path <- file.path(experiment_params$out_path,
                       "data/simulations/datasets",
                       dataset_name,
                       paste0("data_", cli_params$idx, ".rds"))
dt <- readRDS(data_path)

##### ---------- Make Stan data ---------- #####

cat(" Configuring Stan data ...\n")

source(file.path(experiment_params$repo_path, "R", "sim_make_stan_data.R"))
stan_data <- sim_make_stan_data(dt,
                                A = 44,
                                strata.scheme = experiment_params$data$strata,
                                model.params = experiment_params$model)

##### ---------- Running Stan models ---------- #####

cat(" Compiling Stan model ...\n")

model_params <- experiment_params$model
model_fname <- paste0(model_params$name, ".stan")
model_path <- file.path(experiment_params$repo_path,
                        "stan_models",
                        model_fname)
model <- cmdstanr::cmdstan_model(model_path)

cat(" Running Stan model ...\n")

fit <- model$sample(
  data = stan_data,
  chains = model_params$chains,
  seed = experiment_params$seed + cli_params$idx,
  iter_warmup = model_params$warmup,
  iter_sampling = model_params$sampling,
  parallel_chains = model_params$chains,
  max_treedepth = model_params$max_treedepth
)

##### ---------- Save fitted models ---------- #####

# Configure export path
export_path <- file.path(experiment_params$out_path,
                         "stan_fits",
                         dataset_name)

# Create directory if it does not exist
if (!file.exists(export_path)) {
  cat(paste("\n Making export directory:", export_path))
  dir.create(export_path, recursive = TRUE)
}

cat(" Saving the fitted model ...\n")

if (str_detect(model_params$name, "hsgp")) {
  model_name <- paste(model_params$name,
                      model_params$hsgp_m1,
                      model_params$hsgp_m2,
                      sep="-")
}
model_name <- paste(model_name, cli_params$idx, sep="-") # Add array job index
fit$save_object(file = file.path(export_path, paste0(model_name, ".rds")))

cat("\n DONE.\n")

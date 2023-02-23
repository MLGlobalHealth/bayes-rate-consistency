# Run Stan models on POLYMOD data

cat("\n---------- Compiling and Running Stan Model ---------- \n")

# Load libraries
library(optparse)
library(yaml)
library(data.table)
library(tidyr)
library(stringr)
library(cmdstanr)

##### ---------- I/O ---------- #####

cat(" Configuring IO...\n")

option_list <- list(
  make_option(c("-i", "--in"), type = "character", default = NA, help = "repository path", dest = "repo_path"),
  make_option(c("-o", "--out"), type = "character", default = NA, help = "output path", dest = "out_path")
)
cli_params <- optparse::parse_args(optparse::OptionParser(option_list = option_list))
config <- yaml::read_yaml(file.path(cli_params$repo_path, "settings/polymod.yml"))

# Path to model
model_path <- file.path(cli_params$repo_path,
                        "stan_models",
                        paste0(config$model$name), ".stan")

# Export path
export_path <- file.path(cli_params$out_path, "stan_fits")
if (!file.exists(export_path)) {
  cat(paste(" Making export directory:", export_path, "\n"))
  dir.create(export_path, recursive = TRUE)
}

cat(" Loading data...\n")
# Load POLYMOD data
polymod <- readRDS(file.path(cli_params$repo_path, "data/POLYMOD/polymod.rds"))

dt_contacts <- as.data.table(polymod$contacts)
dt_offsets <- as.data.table(polymod$offsets)
dt_population <- as.data.table(polymod$population)

## Configure Stan data
cat(" Configuring Stan data ...\n")

source(file.path(cli_params$repo_path, "R/make_stan_data.R"))

stan_data <- make_stan_data(A = 85,
                            C = 85,
                            W = NULL,
                            dt_contacts = dt_contacts,
                            dt_offsets = dt_offsets,
                            dt_population = dt_population,
                            survey = "POLYMOD",
                            model_params = config$model)

cat(" Compiling Stan model ...\n")

# Compile stan program
model <- cmdstanr::cmdstan_model(model_path, force_recompile = TRUE)
model_params <- config$model

cat(" Running Stan model ...\n")

fit <- model$sample(
  data = stan_data,
  chains = model_params$chains,
  seed = config$seed,
  iter_warmup = model_params$warmup,
  iter_sampling = model_params$sampling,
  parallel_chains = model_params$chains,
  max_treedepth = model_params$max_treedepth,
  adapt_delta = model_params$adapt_delta,
  refresh = model_params$refresh
)

cat(" Saving fitted model ...\n")

model_name <- paste("polymod", model_params$name, sep = "-")
fit$save_object(file = file.path(export_path, paste0(model_name, ".rds")))

cat("\n Run Stan ALL DONE.\n")

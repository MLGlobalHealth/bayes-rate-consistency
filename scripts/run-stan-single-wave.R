# Run Stan models on COVIMOD data

cat("\n---------- Compiling and Running Stan Model ----------\n")

# Load libraries
library(optparse)
library(data.table)
library(yaml)
library(stringr)
library(cmdstanr)
library(tidyr)

##### ---------- I/O ---------- #####
cat( "Configuring IO ...\n")
option_list <- list(
  make_option(c("-i", "--in"), type = "character", default = NA, help = "repository path", dest = "repo_path"),
  make_option(c("-o", "--out"), type = "character", default = NA, help = "output path", dest = "out_path"),
  make_option(c("--config"), type = "character", default = NA, help = "configuration file", dest = "config_file")
)
cli_args <- optparse::parse_args(optparse::OptionParser(option_list = option_list))
config <- read_yaml(file.path(getwd(),
                              "config",
                              cli_args$config_file))

# Load data
contact_data <- readRDS(file.path(cli_args$repo_path,
                                  "data",
                                  config$data$path))

contact_data <- list(contacts = contact_data$contacts, offsets = contact_data$offsets, population = contact_data$pop)

# Unpack data
dt_contacts <- contact_data$contacts
dt_offsets <- contact_data$offsets
dt_population <- contact_data$population

# Path to model
model_path <- paste0(file.path(cli_args$repo_path, "stan_models", config$model$name), ".stan")

# Export path
export_path <- file.path(cli_args$out_path, "stan_fits")
if (!file.exists(export_path)) {
  cat(paste(" Making export directory:", export_path, "\n"))
  dir.create(export_path, recursive = TRUE)
}

## Configure Stan data
cat(" Configuring Stan data ...\n")

# Load helpers
source(file.path(cli_args$repo_path, "R/make_stan_data.R"))
stan_data <- make_stan_data(A = config$data$num_participant_age_groups,
                            C = config$data$num_contact_age_groups,
                            dt_contacts = dt_contacts,
                            dt_offsets = dt_offsets,
                            dt_population = dt_population,
                            model_params = config$model,
                            single_wave = TRUE,
                            single_contact_age = config$data$single_contact_age)

cat(" Compiling Stan model ...\n")

# Compile stan program
model <- cmdstanr::cmdstan_model(model_path, force_recompile = TRUE)

cat(" Running Stan model ...\n")

model_params <- config$model
fit <- model$sample(
  data = stan_data,
  chains = model_params$chains,
  parallel_chains = model_params$chains,
  iter_warmup = model_params$warmup,
  iter_sampling = model_params$sampling,
  max_treedepth = model_params$max_treedepth,
  adapt_delta = model_params$adapt_delta,
  refresh = model_params$refresh,
  seed = config$seed
)

cat(" Saving fitted model ...\n")

model_name <- paste(config$model$name,
                    str_remove(cli_args$config_file, ".yml"),
                    sep = "-")
fit$save_object(file = file.path(export_path, paste0(model_name, ".rds")))

cat("\n Run Stan ALL DONE.\n")

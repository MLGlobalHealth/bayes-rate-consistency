cat("\n---------- Compiling and Running Stan Model ----------\n")

# Load libraries
library(optparse)
library(data.table)
library(tidyr)
library(stringr)
library(cmdstanr)

##### ---------- I/O ---------- #####
cat(" Configuring IO...\n")

option_list <- list(
  make_option(c("-i", "--in"), type = "character", default = NA, help = "repository path", dest = "repo_path"),
  make_option(c("-o", "--out"), type = "character", default = NA, help = "output path", dest = "out_path"),
  make_option(c("--config"), type = "character", default = NA, help = "configuration file", dest = "config_file")
)
cli_params <- optparse::parse_args(optparse::OptionParser(option_list = option_list))
config <- yaml::read_yaml(file.path(getwd(),
                                    "config",
                                    cli_params$config_file))

# Path to model
model_path <- file.path(cli_params$repo_path,
                        "stan_models",
                        paste0(config$model$name, ".stan"))

# Export path
export_path <- file.path(cli_params$out_path, "stan_fits")
if (!file.exists(export_path)) {
  cat(paste(" Making export directory:", export_path, "\n"))
  dir.create(export_path, recursive = TRUE)
}

cat(" Loading data...\n")
# Load helpers
source(file.path(cli_params$repo_path, "R/make_stan_data.R"))
# source(file.path(cli_params$repo_path, "R/covimod-utility.R"))

# Load data
covimod <- readRDS(file.path(cli_params$repo_path,
                             "data",
                             config$data$path))

dt_contacts <- covimod$contacts
dt_offsets <- covimod$offsets
dt_population <- covimod$pop

## Configure Stan data
cat(" Configuring Stan data ...\n")

model_params <- config$model
stan_data <- make_stan_data(85,
                            13,
                            dt_contacts,
                            dt_offsets,
                            dt_population,
                            model_params,
                            single_wave = FALSE,
                            waves = config$data$waves)

# initial values
source(file.path(cli_params$repo_path, "R/stratify_age.R"))
dt_population <- stratify_age(dt_population)
dt_population <- dt_population[, .(pop = sum(pop)), by = c("gender", "age_strata")]
dt_baseline <- merge(dt_contacts, dt_population,
                     by.x = c("alter_gender", "alter_age_strata"),
                     by.y = c("gender", "age_strata"))
baseline <- dt_baseline[y > 0, floor(mean( log( y / N ) - log(pop) ))]
model_inits <- function(){ list(beta_0 = baseline + rnorm(1, 0, 0.2)) }

cat(" Compiling Stan model ...\n")

model <- cmdstanr::cmdstan_model(model_path, force_recompile = TRUE)

cat(" Running Stan model ...\n")

fit <- model$sample(
  data = stan_data,
  chains = model_params$chains,
  init = model_inits,
  seed = config$seed,
  iter_warmup = model_params$warmup,
  iter_sampling = model_params$sampling,
  parallel_chains = model_params$chains,
  max_treedepth = model_params$max_treedepth,
  adapt_delta = model_params$adapt_delta,
  refresh = model_params$refresh
)

cat(" Saving the fitted model ...\n")

data_name <- str_split(config$data$path, "/")[[1]][-1]
data_name <- str_remove(data_name, ".rds")
model_name <- paste(model_params$name, data_name, sep = "_")
fit$save_object(file = file.path(export_path, paste0(model_name, ".rds")))

cat("\n Run Stan ALL DONE.\n")

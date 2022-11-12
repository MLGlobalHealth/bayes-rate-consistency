# Run Stan models on POLYMOD data

cat("\n---------- Compiling and Running Stan Model ----------\n")

# Load libraries
library(optparse)
library(data.table)
library(stringr)
library(cmdstanr)

##### ---------- I/O ---------- #####
option_list <- list(
  make_option("--seed", type = "integer", default = 0721,
              help = "Random number seed [default %default]",
              dest = "seed"),
  make_option("--iter_warmup", type = "integer", default = 500,
              help = "HMC warmup iterations [default %default]",
              dest = 'iter.warmup'),
  make_option("--iter_sampling", type = "integer", default = 1500,
              help = "HMC of sampling iterations iterations [default %default]",
              dest = 'iter.sampling'),
  make_option("--chains", type = "integer", default = 4,
              help = "Number of MCMC chains",
              dest = 'chains'),
  make_option("--model", type = "character", default = "lrd-hsgp",
              help = "Name of Stan model",
              dest = 'model.name'),
  make_option("--hsgp_c", type = "double", default = 1.5,
              help = "The boundary inflation of the HSGP prior in any dimension [default \"%default\"]",
              dest = "hsgp_c"),
  make_option("--hsgp_m1", type = "integer", default = 40,
              help = "The number of the HSGP basis functions in any dimension [default \"%default\"]",
              dest = "hsgp_m1"),
  make_option("--hsgp_m2", type = "integer", default = 20,
              help = "The number of the HSGP basis functions in any dimension [default \"%default\"]",
              dest = "hsgp_m2"),
  make_option("--repo_path", type = "character", default = "/rds/general/user/sd121/home/covimod-gp",
              help = "Absolute file path to repository directory, used as long we don t build an R package [default]",
              dest = 'repo.path'),
  make_option("--waves", type = "integer", default = 5,
              help = "The number of waves to include",
              dest = "waves")
)

args <- optparse::parse_args(optparse::OptionParser(option_list = option_list))

# Load helpers
source(file.path(args$repo.path, "R/stan-utility.R"))

# Load data
polymod <- readRDS(file.path(args$repo.path, "data/POLYMOD/polymod.rds"))

dt.cnt <- polymod$contacts
dt.off <- polymod$offsets
dt.pop <- polymod$population

# Path to model
model.path <- paste0(file.path(args$repo.path, "stan_models", args$model.name), ".stan")

# Export path
export.path <- file.path(args$repo.path, "stan_fits")
if (!file.exists(export.path)) {
  cat(paste(" Making export directory:", export.path, "\n"))
  dir.create(export.path)
}

## Configure Stan data
cat(" Configuring Stan data ...")

# Initialize
stan_data <- init_stan_data(A = 85, C = 85)

# Add contact counts
stan_data <- add_contact_vector(stan_data, dt.cnt, survey = "POLYMOD")

# Add N
stan_data <- add_N(stan_data, survey = "POLYMOD")

# Add row major index
stan_data <- add_row_major_idx(stan_data, dt.cnt, survey = "POLYMOD")

# Add participant offsets
stan_data <- add_part_offsets(stan_data, dt.cnt, dt.off, survey = "POLYMOD")

# Add population offsets
stan_data <- add_pop_offsets(stan_data, dt.pop, survey = "POLYMOD")

# Map age to age strata
stan_data <- add_map_age_to_strata(stan_data, survey = "POLYMOD")

# Add Non-nuisance index
stan_data <- add_nn_idx(stan_data)

# Add standardized age indexes
stan_data <- add_std_age_idx(stan_data)

# Add HSGP parameters
stan_data <- add_hsgp_parms(stan_data, args$hsgp_c, args$hsgp_m1, args$hsgp_m2)

cat(" DONE!\n")

cat(" Compiling Stan model ...")
# Compile stan program
model <- cmdstanr::cmdstan_model(model.path, force_recompile = TRUE)
cat(" DONE!\n")

cat(" Running Stan model ...\n")
fit <- model$sample(
  data = stan_data,
  chains = args$chains,
  seed = args$seed,
  refresh = 10,
  iter_warmup = args$iter.warmup,
  iter_sampling = args$iter.sampling,
  parallel_chains = args$chains,
  max_treedepth = 13,
  adapt_delta = 0.90
)
cat(" DONE!\n")

cat(" Saving fitted model ...")
args$model.name <- paste("polymod", args$model.name, sep="-")
fit$save_object(file = file.path(export.path, paste0(args$model.name, ".rds")))
cat(" DONE!\n")

cat("\n Run Stan ALL DONE.\n")

# Run Stan models on COVIMOD data

cat("\n---------- Compiling and Running Stan Model ----------\n")

# Load libraries
library(optparse)
library(data.table)
library(stringr)
library(cmdstanr)

##### ---------- I/O ---------- #####
option_list <- list(
  optparse::make_option("--seed", type = "integer", default = 0721,
                        help = "Random number seed [default %default]",
                        dest = "seed"),
  optparse::make_option("--iter_warmup", type = "integer", default = 500,
                        help = "HMC warmup iterations [default %default]",
                        dest = 'iter.warmup'),
  optparse::make_option("--iter_sampling", type = "integer", default = 1000,
                        help = "HMC of sampling iterations iterations [default %default]",
                        dest = 'iter.sampling'),
  optparse::make_option("--chains", type = "integer", default = 8,
                        help = "Number of MCMC chains",
                        dest = 'chains'),
  optparse::make_option("--model", type = "character", default = "rd-hsgp",
                        help = "Name of Stan model",
                        dest = 'model.name'),
  optparse::make_option("--hsgp_c", type = "double", default = 1.5,
                        help = "The boundary inflation of the HSGP prior in any dimension [default \"%default\"]",
                        dest = "hsgp_c"),
  optparse::make_option("--hsgp_m", type = "integer", default = 20,
                        help = "The number of the HSGP basis functions in any dimension [default \"%default\"]",
                        dest = "hsgp_m"),
  optparse::make_option("--repo_path", type = "character", default = "/rds/general/user/sd121/home/covimod-gp",
                        help = "Absolute file path to repository directory, used as long we don t build an R package [default]",
                        dest = 'repo.path'),
  optparse::make_option("--wave", type = "integer", default = 1,
                        help = "COVIMOD wave",
                        dest = "wave")
)

args <- optparse::parse_args(optparse::OptionParser(option_list = option_list))

# Load helpers
source(file.path(args$repo.path, "R/stan-utility.R"))

# Load data
covimod <- readRDS(file.path(args$repo.path, "data/COVIMOD/COVIMOD-single.rds"))

dt.cnt <- covimod$contacts[wave == args$wave]
dt.offsets <- covimod$offsets[wave == args$wave]
dt.pop <- covimod$pop

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
stan_data <- init_stan_data()

# Add contact counts
stan_data <- add_contact_vector(stan_data, dt.cnt, dt.offsets)

# Add obs counts
stan_data <- add_N(stan_data)

# Add row major index
stan_data <- add_row_major_idx(stan_data, dt.offsets)

# Add participant offsets
stan_data <- add_part_offsets(stan_data, dt.offsets)

# Add population offsets
stan_data <- add_pop_offsets(stan_data, dt.pop)

# Map age to age strata
stan_data <- add_map_age_to_strata(stan_data)

# Add Non-nuisance index
stan_data <- add_nn_idx(stan_data)

# Add standardized age indexes
stan_data <- add_std_age_idx(stan_data)

# Add HSGP parameters
stan_data <- add_hsgp_parms(stan_data, args$hsgp_c, args$hsgp_m)
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
  refresh = 100,
  iter_warmup = args$iter.warmup,
  iter_sampling = args$iter.sampling,
  parallel_chains = args$chains,
  max_treedepth = 13
)
cat(" DONE!\n")

cat(" Saving fitted model ...")
args$model.name <- paste(args$model.name, args$wave, sep="-")
fit$save_object(file = file.path(export.path, paste0(args$model.name, ".rds")))
cat(" DONE!\n")

cat("\n Run Stan ALL DONE.\n")

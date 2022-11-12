# Run Stan models on COVIMOD data
# For detailed documentation, refer the the run-stan notebook

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
  optparse::make_option("--iter_sampling", type = "integer", default = 1500,
                        help = "HMC of sampling iterations iterations [default %default]",
                        dest = 'iter.sampling'),
  optparse::make_option("--chains", type = "integer", default = 4,
                        help = "Number of MCMC chains",
                        dest = 'chains'),
  optparse::make_option("--model", type = "character", default = "hsgp-m52-lrd",
                        help = "Name of Stan model",
                        dest = 'model.name'),
  optparse::make_option("--hsgp_c", type = "double", default = 1.5,
                        help = "The boundary inflation of the HSGP prior in any dimension [default \"%default\"]",
                        dest = "hsgp_c"),
  optparse::make_option("--hsgp_m1", type = "integer", default = 20,
                        help = "The number of the HSGP basis functions for the difference in age dimension [default \"%default\"]",
                        dest = "hsgp_m1"),
  optparse::make_option("--hsgp_m2", type = "integer", default = 20,
                        help = "The number of the HSGP basis functions for the contacts' age dimension [default \"%default\"]",
                        dest = "hsgp_m2"),
  optparse::make_option("--repo_path", type = "character", default = "/rds/general/user/sd121/home/covimod-gp",
                        help = "Absolute file path to repository directory, used as long we don t build an R package [default]",
                        dest = 'repo.path'),
  optparse::make_option("--waves", type = "integer", default = 5,
                        help = "The number of waves to include",
                        dest = "waves")
)

args <- optparse::parse_args(optparse::OptionParser(option_list = option_list))
# args$repo.path <- "~/Imperial/covimod-gp"

# Load helpers
source(file.path(args$repo.path, "R/stan-utility.R"))
source(file.path(args$repo.path, "R/covimod-utility.R"))

# Load data
covimod <- readRDS(file.path(args$repo.path, "data/COVIMOD/COVIMOD-multi.rds"))

dt.cnt <- covimod$contacts[wave <= args$waves]
dt.offsets <- covimod$offsets[wave <= args$waves]
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
stan_data <- init_stan_data(T = args$waves)

# Add contact counts
stan_data <- add_contact_vector(stan_data, dt.cnt)

# Add N
stan_data <- add_N(stan_data)

# Add row major index
stan_data <- add_row_major_idx(stan_data, dt.cnt)

# Add max index
stan_data <- add_start_end_idx(stan_data, dt.cnt)

# Add participant size offsets
stan_data <- add_part_offsets(stan_data, dt.cnt)

# Add population offsets
stan_data <- add_pop_offsets(stan_data, dt.pop)

# Add map T & R to U
stan_data <- add_map_tr_to_u(stan_data)

# Map age to age strata
stan_data <- add_map_age_to_strata(stan_data)

# Add Non-nuisance index
stan_data <- add_nn_idx(stan_data)

# Add standardized age indexes
stan_data <- add_std_age_idx(stan_data)

# Add HSGP parameters
stan_data <- add_hsgp_parms(stan_data, args$hsgp_c, args$hsgp_m1, args$hsgp_m2)

# initial values
dt.pop <- age_stratify(dt.pop)
dt.pop <- dt.pop[, .(pop = sum(pop)), by=c("gender", "age_strata")]
tmp <- merge(dt.cnt, dt.pop, by.x=c("alter_gender", "alter_age_strata"), by.y=c("gender", "age_strata"))
tmp <- tmp[y > 0, floor(mean( log( y / N ) - log(pop) ))]
model_inits <- function(){ list(beta_0 = tmp + rnorm(1, 0, 0.2)) }
cat(" DONE!\n")

cat(" Compiling Stan model ...")
# Compile stan program
model <- cmdstanr::cmdstan_model(model.path, force_recompile = TRUE)
cat(" DONE!\n")

cat(" Running Stan model ...\n")
fit <- model$sample(
  data = stan_data,
  chains = args$chains,
  init = model_inits,
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
args$model.name <- paste(args$model.name, args$waves, sep="-")
fit$save_object(file = file.path(export.path, paste0(args$model.name, ".rds")))
cat(" DONE!\n")

cat("\n Run Stan ALL DONE.\n")

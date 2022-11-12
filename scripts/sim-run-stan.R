# Run Stan models
cat("\n---------- Compiling and Running Stan Model ----------\n")

# Import libraries
library(optparse)
library(data.table)
library(stringr)
library(cmdstanr)

##### ---------- I/O ---------- #####
option_list <- list(
  optparse::make_option("--seed", type = "integer", default = 1527,
                        help = "Random number seed [default %default]",
                        dest = "seed"),
  optparse::make_option("--iter_warmup", type = "integer", default = 500,
                        help = "HMC warmup iterations [default %default]",
                        dest = 'iter_warmup'),
  optparse::make_option("--iter_sampling", type = "integer", default = 1500,
                        help = "HMC of sampling iterations iterations [default %default]",
                        dest = 'iter_sampling'),
  optparse::make_option("--chains", type = "integer", default = 4,
                        help = "Number of MCMC chains",
                        dest = 'chains'),
  optparse::make_option("--model", type = "character", default = NA_character_,
                        help = "Name of Stan model",
                        dest = 'model'),
  optparse::make_option("--hsgp_binf", type = "double", default = 1.5,
                        help = "The boundary inflation of the HSGP prior in any dimension [default \"%default\"]",
                        dest = "hsgp_binf"),
  optparse::make_option("--hsgp_m1", type = "integer", default = 20,
                        help = "The number of HSGP basis functions in the participant dimension [default \"%default\"]",
                        dest = "hsgp_m1"),
  optparse::make_option("--hsgp_m2", type = "integer", default = 20,
                        help = "The number of HSGP basis functions in the contacts dimension",
                        dest = "hsgp_m2"),
  optparse::make_option("--repo_path", type = "character", default = "/rds/general/user/sd121/home/covimod-gp",
                        help = "Absolute file path to repository directory, used as long we don t build an R package [default]",
                        dest = 'repo.path'),
  optparse::make_option("--data", type = "character", default = NA_character_,
                        help = "Name of the data directory under data/simulations/datasets/",
                        dest = "data"),
  optparse::make_option("--idx", type = "integer", default = NA_integer_,
                        help = "PBD_JOB_IDX",
                        dest = "idx")
)

args <- optparse::parse_args(optparse::OptionParser(option_list = option_list))
args$strata <- str_split(args$data, "_")[[1]][3]

#args$repo.path <- "~/Imperial/covimod-gp"
#args$model <- "cd-hsgp"
#args$data <- "inCOVID_2000_5yr"
#args$idx <- 1


##### ---------- Error handling ---------- #####
if (is.na(args$repo.path)) {
  stop("Please specify --repo_path !")
}

if(is.na(args$data)){
  stop("Please specify --data")
}

if(is.na(args$model)){
  stop("--model is not specified")
}

##### ---------- Configuring Stan data ---------- #####
cat(" Configuring Stan data ...\n")

source( file.path(args$repo.path, "R", "sim-stan-utility.R") ) # Helper functions
data.path <- file.path(args$repo.path, "data/simulations/datasets", args$data, paste0("data_",args$idx,".rds"))
dt <- readRDS(data.path)

##### ---------- Make Stan data ---------- #####
stan_data <- init_stan_data(strata = args$strata)
stan_data <- add_N(stan_data, dt)
stan_data <- add_contacts(stan_data, dt)
stan_data <- add_row_major_idx(stan_data, dt)
stan_data <- add_partsize_offsets(stan_data, dt)
stan_data <- add_group_offsets(stan_data)
stan_data <- add_pop_offsets(stan_data, dt)
stan_data <- add_map_age_to_strata(stan_data, strata = args$strata)
stan_data <- add_std_age_idx(stan_data)

if (stringr::str_detect(args$model, "rd")){
  stan_data <- add_nn_idx(stan_data)
}

if (stringr::str_detect(args$model, "hsgp")){
  stan_data <- add_hsgp_parms(stan_data, args$hsgp_binf, args$hsgp_m1, args$hsgp_m2)
}

##### ---------- Set initial values ---------- #####
set_inits <- function(){
  set.seed(args$seed)
  beta_0 <- dt[, floor(mean(y_strata / part - log(pop_strata)))]

  list(
    beta_0 = beta_0 + rnorm(4, 0, 0.2)
  )
}

##### ---------- Running Stan models ---------- #####
cat(" Compiling Stan model ...\n")
model.path <- paste0(file.path(args$repo.path, "stan_models", args$model), ".stan")
model <- cmdstanr::cmdstan_model(model.path)

cat(" Running Stan model ...\n")
fit <- model$sample(
  data = stan_data,
  chains = args$chains,
  seed = args$seed,
  iter_warmup = args$iter_warmup,
  iter_sampling = args$iter_sampling,
  parallel_chains = args$chains,
  max_treedepth = 13
  # init = set_inits
)

# Export path
export.path <- file.path(args$repo.path, "stan_fits", args$data)
if (!file.exists(export.path)) {
  cat(paste("\n Making export directory:", export.path))
  dir.create(export.path)
}

cat(" Saving fitted model ...\n")
if (stringr::str_detect(args$model, "hsgp")){
  args$model <- paste(args$model, args$hsgp_m1, args$hsgp_m2, sep="-") # Add number of basis functions
}
args$model <- paste(args$model, args$idx, sep="_") # Add array job index
fit$save_object(file = file.path(export.path, paste0(args$model, ".rds")))

cat("\n DONE.\n")

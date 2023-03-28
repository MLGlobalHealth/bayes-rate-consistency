# Preamble: Generates diagnostic statistics and result plots

cat("\n ---------- Begin Post-processing ---------- \n")

# Load libraries
library(optparse)
library(yaml)
library(data.table)
library(cmdstanr)
library(bayesplot)
library(loo)
library(reshape2)
library(stringr)
library(ggplot2)
library(pammtools)

bayesplot::color_scheme_set(scheme = "mix-blue-pink")

options(mc.cores = 4)

##### ---------- I/O ---------- #####
cat(" Loading experiment parameters ...\n")

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

# Read experiment settings
experiment_params <- read_yaml(file.path(getwd(),
                                         "config",
                                         cli_params$config_file))

# Source helpers
repo_path <- cli_params$repo_path
source(file.path(repo_path, "R/convergence_diagnostic_stats.R"))
source(file.path(repo_path, "R/sim_posterior_predictive_check.R"))
source(file.path(repo_path, "R/sim_posterior_contact_intensity.R"))
source(file.path(repo_path, "R/sim_prediction_error_table.R"))

data_params <- experiment_params$data
dataset_name <- paste(ifelse(data_params$covid, "inCOVID", "preCOVID"),
                      data_params$size,
                      data_params$strata,
                      sep = "_")

model_params <- experiment_params$model

if (str_detect(model_params$name, "hsgp")) {
  model_name <- paste(model_params$name,
                      model_params$hsgp_m1,
                      model_params$hsgp_m2,
                      sep = "-")
} else if (str_detect(model_params$name, "2dgp")) {
  model_name <- model_params$name
}

fit_path <- file.path(cli_params$out_path,
                      "stan_fits",
                      dataset_name,
                      paste0(model_name, "-", cli_params$pidx, ".rds"))

data_path <- file.path(cli_params$out_path,
                       "data/simulations/datasets",
                       dataset_name,
                       paste0("data_", cli_params$pidx, ".rds"))

# Error handling
if (!file.exists(fit_path)) {
  cat("\n Model: ", fit_path)
  stop("The specified model does not exist!")
}
if (!file.exists(data_path)) {
  stop("The specified dataset does not exists!")
}

# Create export directory if it does not exist
export_path <- file.path(cli_params$out_path,
                         "results",
                         dataset_name,
                         paste(model_name, cli_params$pidx, sep = "-"))
export_fig_path <- file.path(export_path, "figures")
if (!dir.exists(export_path)) {
  dir.create(export_path, recursive = TRUE)
  dir.create(export_fig_path)
} else {
  if (!dir.exists(export_fig_path)) {
    dir.create(export_fig_path)
  }
}

##### ---------- Setup ---------- #####
fit <- readRDS(fit_path)
dt <- readRDS(data_path)
dt_population <- unique(dt[, .(alter_age, alter_gender, pop)])

##### ---------- Assess convergence and mixing ---------- #####
cat("\n Assess convergence and mixing...\n")

# Make convergence diagnostic tables
fit_summary <- convergence_diagnostic_stats(fit, outdir=export_path)

# Make trace plots
cat(" Making trace plots\n")

pars <- c('nu', 'gp_alpha', 'gp_rho_1', 'gp_rho_2')
pars_po <- fit$draws(pars)

p <- bayesplot::mcmc_trace(pars_po)
ggsave(file = file.path(export_fig_path, "trace.pdf"),
       plot = p,
       h = 20, w = 20,
       limitsize = F)

# Make pairs plots
cat(" Making pairs plots\n")

p <- bayesplot::mcmc_pairs(pars_po, off_diag_args=list(size=0.3, alpha=0.3))
ggsave(file = file.path(export_fig_path, "pairs.pdf"),
       plot = p,
       h = 20, w = 20,
       limitsize = F)

##### ---------- Posterior predictive checks ---------- #####

cat(" Posterior predictive checks...\n")

dt_ppc <- sim_posterior_predictive_check(fit, dt, outdir = export_path)

#### ----------- Extract posterior intensities ---------- #####

cat(" Extracting posterior intensities...\n")

dt_matrix <- sim_posterior_contact_intensity(fit,
                                             dt_population,
                                             type = "matrix",
                                             outdir = export_path)

dt_margin <- sim_posterior_contact_intensity(fit,
                                             dt_population,
                                             type = "marginal",
                                             outdir = export_path)

##### --------- Mean Squared Error ---------- #####

cat(" Assessing Prediction error...\n")

sim_prediction_error_table(dt, dt_matrix, outdir = export_path)

cat("\n DONE.\n")

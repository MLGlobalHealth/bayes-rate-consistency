# Preamble: Generates diagnostic statistics and result plots

cat("\n ---------- Begin Post-processing ---------- \n")

library(optparse)
library(data.table)
library(yaml)
library(cmdstanr)
library(bayesplot)
library(loo)
library(reshape2)
library(stringr)
library(ggplot2)
library(viridis)
library(pammtools)

bayesplot::color_scheme_set(scheme = "mix-blue-pink")

##### ---------- I/O ---------- #####
option_list <- list(
  make_option(c("-i", "--in"), type = "character", default = NA, help = "repository path", dest = "repo_path"),
  make_option(c("-o", "--out"), type = "character", default = NA, help = "output path", dest = "out_path"),
  make_option(c("--config"), type = "character", default = NA, help = "configuration file", dest = "config_file")
)
cli_args <- parse_args(OptionParser(option_list = option_list))

config <- read_yaml(file.path(getwd(), "config", cli_args$config_file))
model_name <- paste(config$model$name,
                    str_remove(cli_args$config_file, ".yml"),
                    sep = "-")
model_path <- file.path(cli_args$out_path, "stan_fits", paste0(model_name, ".rds"))
data_path <- file.path(getwd(), "data", config$data$path)

# Error handling
if (!file.exists(model_path)) {
  cat("\n Model: ", model_path)
  stop("The specified model does not exist!")
}

if (!file.exists(data_path)) {
  stop("The specified dataset does not exists!")
}

# Output directories
export_path <- file.path(cli_args$out_path, "results", model_name)
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
cat(paste("\n Model path:", model_path))
cat(paste("\n Data path:", data_path), "\n\n")

fit <- readRDS(model_path)
data <- readRDS(data_path)

# Unpack data
dt_contacts <- data$contacts
dt_offsets <- data$offsets
dt_population <- data$population

# Load helpers
source(file.path(cli_args$repo_path, "R/convergence_diagnostic_stats.R"))
source(file.path(cli_args$repo_path, "R/posterior_predictive_check.R"))
source(file.path(cli_args$repo_path, "R/posterior_log_contact_rates.R"))
source(file.path(cli_args$repo_path, "R/posterior_contact_intensity.R"))

##### ---------- Assess convergence and mixing ---------- #####
cat(" Assess convergence and mixing ...\n")

# Make convergence diagnostic tables
fit_summary <- convergence_diagnostic_stats(fit, outdir = export_path)

# Make trace plots
cat("\n Making trace plots")

pars <- c('nu', 'gp_alpha', 'gp_rho_1', 'gp_rho_2')
pars_po <- fit$draws(pars)
p <- bayesplot::mcmc_trace(pars_po)
ggsave(file = file.path(export_fig_path, 'mcmc_trace_parameters.png'),
       plot = p,
       h = 20,
       w = 20,
       limitsize = F)

# Make pairs plots
cat(" Making pairs plots\n")
p <- bayesplot::mcmc_pairs(pars_po, off_diag_args = list(size = 0.3, alpha = 0.3))
ggsave(file = file.path(export_fig_path, 'mcmc_pairs_parameters.png'),
       plot = p,
       h = 20,
       w = 20,
       limitsize = F)

##### ---------- Posterior predictive checks ---------- #####
cat(" Extracting posterior ...\n")
posterior_draws <- fit$draws(c("yhat_strata", "log_cnt_rate"),
                             inc_warmup = FALSE,
                             format = "draws_matrix")

cat(" Making posterior predictive checks ...\n")

dt_ppc <- posterior_predictive_check(posterior_draws,
                                     dt_contacts,
                                     outdir = export_path)

cat(" Extracting posterior contact intensities ...\n")

dt_posterior <- posterior_log_contact_rates(posterior_draws)

dt_matrix <- posterior_contact_intensity(dt_posterior,
                                         dt_population,
                                         type = "matrix",
                                         outdir = export_path)

dt_margin <- posterior_contact_intensity(dt_posterior,
                                         dt_population,
                                         type = "marginal",
                                         outdir = export_path)

cat("\n DONE.\n")


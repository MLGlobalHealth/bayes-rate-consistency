# Preamble: Generates diagnostic statistics and result plots

cat("\n ---------- Begin Post-processing ---------- \n")

library(optparse)
library(data.table)
library(cmdstanr)
library(bayesplot)
library(loo)
library(reshape2)
library(stringr)
library(ggplot2)
library(viridis)
library(pammtools)

##### ---------- I/O ---------- #####
option_list <- list(
  optparse::make_option("--repo_path", type = "character", default = "/rds/general/user/sd121/home/covimod-gp",
                        help = "Absolute file path to repository directory, used as long we don t build an R package [default]",
                        dest = "repo.path"),
  optparse::make_option("--wave", type="integer", default = 1,
                        help = "COVIMOD wave",
                        dest = "wave"),
  optparse::make_option("--model", type = "character", default = NA_character_,
                        help = "Name of the model",
                        dest = "model.name"),
  optparse::make_option("--mixing", type = "logical", default = TRUE,
                        help = "Whether to assess mixing",
                        dest = "mixing"),
  optparse::make_option("--ppc", type = "logical", default = TRUE,
                        help = "Whether to run posterior predictive checks",
                        dest = "ppc"),
  optparse::make_option("--plot", type = "logical", default = TRUE,
                        help = "Whether to plot posterior distributions",
                        dest = "plot")
)

args <- optparse::parse_args(optparse::OptionParser(option_list = option_list))

model.path <- file.path(args$repo.path, "stan_fits", paste0(args$model.name, ".rds"))
data.path <- file.path(args$repo.path, "data/COVIMOD/COVIMOD-single.rds")

# Error handling
if(!file.exists(model.path)) {
  cat("\n Model: ", model.path)
  stop("The specified model does not exist!")
}
if(!file.exists(data.path)) {
  stop("The specified dataset does not exists!")
}

# Output directories
export.path <- file.path(args$repo.path, "results", args$model.name)
export.fig.path <- file.path(export.path, "figures")
if(!dir.exists(export.path)){
  dir.create(export.path, recursive = TRUE)
  dir.create(export.fig.path)
} else {
  if(!dir.exists(export.fig.path)){
    dir.create(export.fig.path)
  }
}

##### ---------- Setup ---------- #####
cat(paste("\n Model path:", model.path))
cat(paste("\n Data path:", data.path))

fit <- readRDS(model.path)
data <- readRDS(data.path)
dt.cnt <- data$contacts[wave == args$wave]
dt.offsets <- data$offsets[wave == args$wave]
dt.pop <- data$pop

source(file.path(args$repo.path, "R/covimod-utility.R"))
source(file.path(args$repo.path, "R/stan-utility.R"))
source(file.path(args$repo.path, "R/postprocess-diagnostic-single.R"))
source(file.path(args$repo.path, "R/postprocess-plotting-single.R"))

##### ---------- Assess convergence and mixing ---------- #####
if(args$mixing){
  cat(" Assess convergence and mixing\n")

  # Make convergence diagnostic tables
  fit_summary <- make_convergence_diagnostic_stats(fit, outdir=export.path)

  # Make trace plots
  cat("\n Making trace plots")
  bayesplot::color_scheme_set(scheme = "mix-blue-pink")

  pars <- c('nu', 'gp_alpha', 'gp_rho_1', 'gp_rho_2')

  pars_po <- fit$draws(pars)
  p <- bayesplot::mcmc_trace(pars_po)
  ggsave(file = file.path(export.fig.path, 'mcmc_trace_parameters.png'), plot = p, h = 20, w = 20, limitsize = F)

  # Make pairs plots
  cat(" Making pairs plots\n")
  p <- bayesplot::mcmc_pairs(pars_po, off_diag_args=list(size=0.3, alpha=0.3))
  ggsave(file = file.path(export.fig.path, 'mcmc_pairs_parameters.png'), plot = p, h = 20, w = 20, limitsize = F)

  cat("\n DONE!\n")
}

##### ---------- Posterior predictive checks ---------- #####
if(args$ppc){
  cat(" Extracting posterior\n")
  po <- fit$draws(c("yhat_strata", "log_cnt_rate"), inc_warmup = FALSE, format="draws_matrix")

  cat(" Making posterior predictive checks\n")
  make_ppd_check_covimod(po, dt.cnt, outdir=export.path)

  cat("\n DONE.\n")
}

##### ---------- Plotting ---------- #####
if(args$plot){
  cat(" Extracting posterior contact intensities\n")
  dt.po <- extract_posterior_rates(po)
  dt.matrix <- posterior_contact_intensity(dt.po, dt.pop, type="matrix", outdir=export.path)
  dt.margin <- posterior_contact_intensity(dt.po, dt.pop, type="marginal", outdir=export.path)

  rm(dt.po); suppressMessages(gc()); # Ease memory

  cat(" Making figures\n")

  p <- plot_posterior_intensities(dt.matrix, outdir=export.path)
  p <- plot_sliced_intensities(dt.matrix, outdir=export.path)
  p <- plot_marginal_intensities(dt.margin, outdir=export.path)

  cat("\n DONE.\n")
}


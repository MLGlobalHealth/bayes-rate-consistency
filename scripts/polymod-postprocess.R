# Preamble: Generates diagnostic statistics and result plots

cat("\n ---------- Begin Post-processing ---------- \n")

library(optparse)
library(data.table)
library(cmdstanr)
library(ggplot2)
library(bayesplot)
library(loo)
library(reshape2)
library(stringr)

##### ---------- I/O ---------- #####
option_list <- list(
  optparse::make_option("--repo_path", type = "character", default = "/rds/general/user/sd121/home/covimod-gp",
                        help = "Absolute file path to repository directory [default]",
                        dest = 'repo.path'),
  optparse::make_option("--model", type = "character", default = NA_character_,
                        help = "Name of the model",
                        dest = "model"),
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

model.path <- file.path(args$repo.path, "stan_fits", paste0("polymod-", args$model, ".rds"))
data.path <- file.path(args$repo.path, "data/POLYMOD/polymod.rds")

# Error handling
if(!file.exists(model.path)) {
  cat("\n Model: ", model.path)
  stop("The specified model does not exist!")
}
if(!file.exists(data.path)) {
  stop("The specified dataset does not exists!")
}

# Output directories
export.path <- file.path(args$repo.path, "results", paste0("polymod-", args$model))
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
options(mc.cores=4)

fit <- readRDS(model.path)
polymod <- readRDS(data.path)
dt.cnt <- polymod$contacts
dt.pop <- polymod$population

source(file.path(args$repo.path, "R/polymod-postprocess-diagnostic.R"))

##### ---------- Assess convergence and mixing ---------- #####
if(args$mixing){
  cat("\n Assess convergence and mixing \n")

  # Make convergence diagnostic tables
  fit_summary <- make_convergence_diagnostic_stats(fit, outdir=export.path)
  pars <- c('nu', 'gp_alpha', 'gp_rho_1', 'gp_rho_2')
  pars_po <- fit$draws(pars)

  # Make trace plots
  cat(" Making trace plots\n")
  bayesplot::color_scheme_set(scheme = "mix-blue-pink")

  p <- bayesplot::mcmc_trace(pars_po)
  ggsave(file=file.path(export.fig.path, "trace.pdf"), plot=p, h=20, w=20, limitsize = F)

  # Make pairs plots
  cat(" Making pairs plots\n")
  p <- bayesplot::mcmc_pairs(pars_po, off_diag_args=list(size=0.3, alpha=0.3))
  ggsave(file=file.path(export.fig.path, "pairs.pdf"), plot=p, h=20, w=20, limitsize = F)
}

##### ---------- Posterior predictive checks ---------- #####
if(args$ppc){
  cat(" Posterior predictive checks\n")
  dt.ppc <- make_ppc(fit, dt.cnt, outdir = export.path) # Make posterior predictive checks
}

#### ----------- Extract posterior intensities ---------- #####
if(args$plot){
  cat(" Extracting posterior intensities\n")
  dt.po <- extract_posterior_intensity(fit)
  dt.matrix <- summarise_posterior_intensity(fit, dt.po, dt.pop, type = "matrix", outdir = export.path)
  dt.margin <- summarise_posterior_intensity(fit, dt.po, dt.pop, type = "marginal", outdir = export.path)
}

cat("\n DONE.\n")

# Preamble: Generates diagnostic statistics and result plots

cat("\n ---------- Begin Post-processing ---------- \n")

library(optparse)
library(data.table)
library(cmdstanr)
library(bayesplot)
library(posterior)
library(reshape2)
library(stringr)
library(ggplot2)
library(viridis)
library(pammtools)

##### ---------- I/O ---------- #####
option_list <- list(
  optparse::make_option("--repo_path", type = "character", default = "/rds/general/user/sd121/home/covimod-gp",
                        help = "Absolute file path to repository directory, used as long we don t build an R package [default]",
                        dest = 'repo.path'),
  optparse::make_option("--model", type = "character", default = NA_character_,
                        help = "Name of the model",
                        dest = "model.name"),
  optparse::make_option("--waves", type = "integer", default = 5,
                        help = "Number of waves",
                        dest = "waves"),
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
data.path <- file.path(args$repo.path, "data/COVIMOD/COVIMOD-multi.rds")

# Error handling
if(!file.exists(model.path)) {
  cat("\n Model: ", model.path)
  stop("The specified model does not exist!")
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
options(mc.cores=10)

cat(paste("\n Model:", model.path, "\n"))

fit <- readRDS(model.path)
data <- readRDS(data.path)
dt.cnt <- data$contacts[wave <= args$waves]
dt.off <- data$offsets[wave <= args$waves]
dt.pop <- data$pop

source(file.path(args$repo.path, "R/covimod-utility.R"))
source(file.path(args$repo.path, "R/stan-utility.R"))
source(file.path(args$repo.path, "R/postprocess-diagnostic.R"))
source(file.path(args$repo.path, "R/postprocess-plotting.R"))

##### ---------- Assess convergence and mixing ---------- #####
if(args$mixing){
  cat(" Assess convergence and mixing ...\n")

  # Make convergence diagnostic tables
  fit_summary <- make_convergence_diagnostic_stats(fit, outdir=export.path)

  # Make trace plots
  cat("  Making trace plots and pairs plots of GP params\n")
  bayesplot::color_scheme_set(scheme = "mix-blue-pink")

  pars <- c('gp_sigma', 'gp_rho_1', 'gp_rho_2')
  pars_po <- fit$draws(pars)
  np <- nuts_params(fit)

  for(w in 1:args$waves){
    .pattern <- paste0(pars, "\\[", w, ",[1-4]\\]")
    p <- bayesplot::mcmc_trace(pars_po, regex_pars = .pattern, facet_args = list(ncol=1), np=np)
    ggsave(file = file.path(export.fig.path, paste0('mcmc_trace_', w, '.png')),
           plot = p, h = 20, w = 20, limitsize = FALSE)

    p <- bayesplot::mcmc_pairs(pars_po, regex_pars = .pattern, off_diag_args=list(size=0.3, alpha=0.3), np=np)
    ggsave(file = file.path(export.fig.path, paste0('mcmc_pairs_', w, '.png')),
           plot = p, h = 20, w = 20, limitsize = FALSE)
  }

  cat(" DONE!\n")
}

##### ---------- Posterior predictive checks ---------- #####
if(args$ppc){
  cat(" Making posterior predictive checks ...")
  make_ppd_check(fit$draws("yhat_strata", inc_warmup = FALSE, format="draws_matrix"),
                 dt.cnt, dt.offsets, outdir=export.path)
  cat(" DONE!\n")
}

##### ---------- Plotting ---------- #####
if(args$plot){
  cat(" Extracting posterior estimates ...\n")

  # cat("  Summarising GP parameters ...\n")
  # po_gp <- fit$draws(c("gp_sigma", "gp_rho_1", "gp_rho_2"), inc_warmup = FALSE)
  # gp_parms_summary <- summarise_draws(po_gp, ~quantile(.x, probs = c(0.025, 0.25, 0.5, 0.75, 0.975)))
  # saveRDS(gp_parms_summary, file = file.path(export.path, "gp_parms_summary.rds"))

  po <- tryCatch(
    { # If time effects are present
      po <- fit$draws(c("log_cnt_rate", "tau", "rho"), inc_warmup = FALSE, format="draws_matrix")
      cat(" Plotting time and repeat effects ...\n")
      # Plot betas
      plot_time_effects(po, export.path)
      plot_repeated_response_effects(po, export.path)
      plot_time_repeat_pairs(po, export.path)
      po_tr <- subset_draws(po, variable = c("tau", "rho"), regex = TRUE)
      time_rep_summary <- summarise_draws(po_tr, ~quantile(.x, probs = c(0.025, 0.25, 0.5, 0.75, 0.975)))
      saveRDS(time_rep_summary, file = file.path(export.path, "time_rep_summary.rds"))
      return(po)
    },
    error = function(e){
      message("Repeat effects do not exist.")
      po <- fit$draws(c("log_cnt_rate", "tau"), inc_warmup = FALSE, format="draws_matrix")
      time_rep_summary <- summarise_draws(subset_draws(po, variable = "tau"),
                                          ~quantile(.x, probs = c(0.025, 0.25, 0.5, 0.75, 0.975)))
      saveRDS(time_rep_summary, file = file.path(export.path, "time_rep_summary.rds"))
      plot_time_effects(po, export.path)
      return(po)
    }
  )

  cat(" Extracting posterior contact intensities ...\n")
  dt.po <- extract_posterior_intensity(po, dt.pop)

  #dt.matrix <- summarise_posterior_intensity(dt.po, type="matrix", outdir=export.path)
  #dt.sliced <- summarise_posterior_intensity(dt.po, dt.off, type="sliced", outdir=export.path)
  dt.margin.a <- summarise_posterior_intensity(dt.po, type="margin-a", outdir=export.path)
  #dt.margin.b <- summarise_posterior_intensity(dt.po, type="margin-b", outdir=export.path)
  #dt.margin.c <- summarise_posterior_intensity(dt.po, dt.off, type="margin-c", outdir=export.path)
}

cat("\n DONE.\n")

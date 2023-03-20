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

data_path <- file.path(cli_params$repo_path,
                       "data",
                       config$data$path)
data_name <- str_remove(tail(str_split(data_path, "/")[[1]], 1), ".rds")

model_name <- paste(config$model$name, data_name, sep = "_")
model_path <- file.path(cli_params$out_path,
                        "stan_fits",
                        paste0(model_name, ".rds"))

# Error handling
if (!file.exists(model_path)) {
  cat("\n Model: ", model_path)
  stop("The specified model does not exist!")
}

# Output directories
export_path <- file.path(cli_params$out_path, "results", model_name)
export_fig_path <- file.path(export_path, "figures")
if (!dir.exists(export_path)) {
  dir.create(export_path, recursive = TRUE)
  dir.create(export_fig_path)
} else {
  if (!dir.exists(export_fig_path)) {
    dir.create(export_fig_path, recursive = TRUE)
  }
}

##### ---------- Setup ---------- #####
options(mc.cores = 10)

cat(paste("\n Model:", model_path, "\n"))

fit <- readRDS(model_path)
data <- readRDS(data_path)
dt_contacts <- data$contacts
dt_offsets <- data$offsets
dt_population <- data$pop

source(file.path(cli_params$repo_path, "R", "make_stan_data.R"))
source(file.path(cli_params$repo_path, "R", "postprocess-diagnostic.R"))
source(file.path(cli_params$repo_path, "R", "postprocess-plotting.R"))

##### ---------- Assess convergence and mixing ---------- #####
if (config$postprocess$mixing) {
  cat(" Assess convergence and mixing ...\n")

  # Make convergence diagnostic tables
  fit_summary <- make_convergence_diagnostic_stats(fit, outdir = export_path)

  # Make trace plots
  cat("  Making trace plots and pairs plots of GP params\n")
  bayesplot::color_scheme_set(scheme = "mix-blue-pink")

  pars <- c('gp_sigma', 'gp_rho_1', 'gp_rho_2')
  pars_po <- fit$draws(pars)
  np <- nuts_params(fit)

  for (w in 1:config$data$waves) {
    .pattern <- paste0(pars, "\\[", w, ",[1-4]\\]")
    p <- bayesplot::mcmc_trace(pars_po,
                               regex_pars = .pattern,
                               facet_args = list(ncol=1),
                               np = np)
    ggsave(file = file.path(export_fig_path, paste0('mcmc_trace_', w, '.png')),
           plot = p, h = 20, w = 20, limitsize = FALSE)

    p <- bayesplot::mcmc_pairs(pars_po,
                               regex_pars = .pattern,
                               off_diag_args = list(size = 0.3, alpha = 0.3),
                               np = np)
    ggsave(file = file.path(export_fig_path, paste0('mcmc_pairs_', w, '.png')),
           plot = p, h = 20, w = 20, limitsize = FALSE)
  }
}

##### ---------- Posterior predictive checks ---------- #####
if (config$postprocess$ppc) {
  cat(" Making posterior predictive checks ...\n")
  make_ppd_check(fit$draws("yhat_strata",
                           inc_warmup = FALSE,
                           format = "draws_matrix"),
                 dt_contacts,
                 dt_offsets,
                 outdir = export_path)
}

##### ---------- Plotting ---------- #####
if (config$postprocess$plot) {
  cat(" Extracting posterior estimates ...\n")

  cat("  Summarising GP parameters ...\n")
  po_gp <- fit$draws(c("gp_sigma", "gp_rho_1", "gp_rho_2"), inc_warmup = FALSE)
  gp_parms_summary <- summarise_draws(po_gp, ~quantile(.x, probs = c(0.025, 0.25, 0.5, 0.75, 0.975)))
  saveRDS(gp_parms_summary,
          file = file.path(export_path, "gp_parms_summary.rds"))

  po <- tryCatch(
    { # If time effects are present
      po <- fit$draws(c("log_cnt_rate", "tau", "rho"),
                      inc_warmup = FALSE,
                      format = "draws_matrix")

      cat(" Plotting time and repeat effects ...\n")
      # Plot betas
      plot_time_effects(po, export_path)
      plot_repeated_response_effects(po, export_path)
      plot_time_repeat_pairs(po, export_path)
      po_tr <- subset_draws(po, variable = c("tau", "rho"), regex = TRUE)
      time_rep_summary <- summarise_draws(po_tr, ~quantile(.x, probs = c(0.025, 0.25, 0.5, 0.75, 0.975)))
      saveRDS(time_rep_summary, file = file.path(export_path, "time_rep_summary.rds"))
      return(po)
    },
    error = function(e){
      message("Repeat effects do not exist.")
      po <- fit$draws(c("log_cnt_rate", "tau"), inc_warmup = FALSE, format="draws_matrix")
      time_rep_summary <- summarise_draws(subset_draws(po, variable = "tau"),
                                          ~quantile(.x, probs = c(0.025, 0.25, 0.5, 0.75, 0.975)))
      saveRDS(time_rep_summary, file = file.path(export_path, "time_rep_summary.rds"))
      plot_time_effects(po, export_path)
      return(po)
    }
  )

  cat(" Extracting posterior contact intensities ...\n")
  dt_posterior <- extract_posterior_intensity(po, dt_population)

  dt_matrix <- summarise_posterior_intensity(dt_posterior,
                                             type = "matrix",
                                             outdir = export_path)

  dt_sliced <- summarise_posterior_intensity(dt_posterior,
                                             dt_offsets,
                                             type = "sliced",
                                             outdir = export_path)

  dt_margin_a <- summarise_posterior_intensity(dt_posterior,
                                               type = "margin-a",
                                               outdir = export_path)

  dt_margin_b <- summarise_posterior_intensity(dt_posterior,
                                               type = "margin-b",
                                               outdir = export_path)

  dt_margin_c <- summarise_posterior_intensity(dt_posterior,
                                               dt_offsets,
                                               type = "margin-c",
                                               outdir = export_path)
}

cat("\n DONE.\n")

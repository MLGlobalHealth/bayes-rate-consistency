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
library(pammtools)

##### ---------- I/O ---------- #####
option_list <- list(
  optparse::make_option("--repo_path", type = "character", default = "/rds/general/user/sd121/home/covimod-gp",
                        help = "Absolute file path to repository directory [default]",
                        dest = 'repo.path'),
  optparse::make_option("--model", type = "character", default = NA_character_,
                        help = "Name of the model",
                        dest = "model"),
  optparse::make_option("--data", type = "character", default = NA_character_,
                        help = "Name of the data directory under data/simulations/datasets/",
                        dest = "data"),
  optparse::make_option("--idx", type = "integer", default = NA_integer_,
                        help = "PBD_JOB_IDX",
                        dest = "idx")
)

args <- optparse::parse_args(optparse::OptionParser(option_list = option_list))

model.path <- file.path(args$repo.path, "stan_fits", args$data, paste0(args$model, "_", args$idx, ".rds"))
data.path <- file.path(args$repo.path, "data/simulations/datasets", args$data, paste0("data_", args$idx, ".rds"))

# Error handling
if(!file.exists(model.path)) {
  cat("\n Model: ", model.path)
  stop("The specified model does not exist!")
}
if(!file.exists(data.path)) {
  stop("The specified dataset does not exists!")
}

# Output directories
export.path <- file.path(args$repo.path, "results", args$data, paste(args$model, args$idx, sep="_"))
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
dt <- readRDS(data.path)
dt.pop <- unique(dt[, list(alter_age, alter_gender, pop)])
setnames(dt.pop, c("alter_age", "alter_gender"), c("age", "gender"))

source(file.path(args$repo.path, "R/sim-postprocess-diagnostic.R"))

##### ---------- Assess convergence and mixing ---------- #####
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

##### ---------- Posterior predictive checks ---------- #####
cat(" Posterior predictive checks\n")
dt.ppc <- make_ppc(fit, dt, outdir = export.path) # Make posterior predictive checks

#### ----------- Extract posterior intensities ---------- #####
cat(" Extracting posterior intensities\n")
dt.matrix <- posterior_contact_intensity(fit, dt.pop, type = "matrix", outdir = export.path)
dt.margin <- posterior_contact_intensity(fit, dt.pop, type = "marginal", outdir = export.path)

##### --------- Mean Squared Error ---------- #####
cat(" Assessing Error\n")
make_error_table(dt, dt.matrix, outdir = export.path)

cat("\n DONE.\n")

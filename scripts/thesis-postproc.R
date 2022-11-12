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

bayesplot::color_scheme_set(scheme = "mix-blue-pink")

##### ---------- I/O ---------- #####
option_list <- list(
  optparse::make_option("--repo_path", type = "character", default = "/rds/general/user/sd121/home/covimod-gp",
                        help = "Absolute file path to repository directory, used as long we don t build an R package [default]",
                        dest = 'repo.path'),
  optparse::make_option("--model", type = "character", default = NA_character_,
                        help = "Name of the model",
                        dest = "model.name")
)

args <- optparse::parse_args(optparse::OptionParser(option_list = option_list))

model.path <- file.path(args$repo.path, "stan_fits", paste0(args$model.name, ".rds"))

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

# Make trace plots
fit <- readRDS(model.path)
po <- fit$draws(variables = c("gp_rho_2[2,1]", "gp_rho_2[3,2]", "gp_rho_2[2,3]", "gp_rho_2[3,3]", "f[2,1,85,73]"))

p <- bayesplot::mcmc_trace(po, facet_args = list(ncol = 1)) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    strip.background = element_blank(),
    strip.text = element_text(size = 8)
  )

ggsave(file = file.path(export.fig.path, paste0('mcmc_trace_min_eff.pdf')),
       plot = p, units = "cm", w = 14.7, h = 16, dpi = 300)

cat("\n DONE.\n")

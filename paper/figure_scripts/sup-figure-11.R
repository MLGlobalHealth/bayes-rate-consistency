library(data.table)
library(bayesplot)
library(posterior)
library(cmdstanr)
library(ggplot2)

cat("========== Making trace plots ==========\n")
cat(" Loading fit summary...\n")
fit_summary <- readRDS("~/bayes-rate-consistency-output/results/hsgp-m52-lrd_COVIMOD_multi_1234/fit_summary.rds")

cat(" Loading Stan fit...\n")
fit <- readRDS("~/bayes-rate-consistency-output/stan_fits/hsgp-m52-lrd_COVIMOD_multi_1234.rds")

# Extract parameters
cat(" Extracting posterior estimates...\n")
par_min_ess <- fit_summary[which.min(fit_summary$ess_bulk), "variable"]
par_max_rhat <- fit_summary[which.max(fit_summary$rhat), "variable"]
posterior_draws <- fit$draws(variables = c(par_min_ess, par_max_rhat))

# Make trace plots
cat(" Making trace plots...\n")
bayesplot::color_scheme_set(scheme = "mix-blue-pink")
plt <- bayesplot::mcmc_trace(posterior_draws,
                             facet_args = list(nrow = 2, ncol = 1)) +
  theme(axis.title = element_text(size = 8),
        axis.text = element_text(size = 8),
        strip.text = element_text(size = 8),
        plot.title = element_text(size = 9),
        plot.subtitle = element_text(size = 8))

cat(" Saving plots...\n")
if (dir.exists("~/bayes-rate-consistency/paper/figures")) {
  dir.create("~/bayes-rate-consistency/paper/figures", recursive = TRUE)
}
ggsave(file = file.path("~/bayes-rate-consistency/paper/figures",
                        'sup-figure-11.jpeg'),
       plot = plt,
       width = 19,
       height = 10.5,
       units = "cm")




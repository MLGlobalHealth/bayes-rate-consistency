library(data.table)
library(bayesplot)
library(posterior)
library(cmdstanr)


fit_summary <- readRDS("~/bayes-rate-consistency-output/results/hsgp-m52-lrd_COVIMOD_multi_1234/fit_summary.rds")
fit <- readRDS("~/bayes-rate-consistency-output/stan_fits/hsgp-m52-lrd_COVIMOD_multi_1234.rds")

# Extract parameters
par_min_ess <- fit_summary[which.min(fit_summary$ess_bulk), "variable"]
par_max_rhat <- fit_summary[which.max(fit_summary$rhat), "variable"]
posterior_draws <- fit$draws(variables = c(par_min_ess, par_max_rhat))

# Make trace plots
bayesplot::color_scheme_set(scheme = "mix-blue-pink")
plt <- bayesplot::mcmc_trace(posterior_draws,
                             facet_args = list(nrow = 2, ncol = 1))

ggsave(file = file.path("~/bayes-rate-consistency/paper/figures",
                        'sup-figure-11.jpeg'),
       plot = plt,
       width = 19,
       height = 10.5,
       units = "cm")




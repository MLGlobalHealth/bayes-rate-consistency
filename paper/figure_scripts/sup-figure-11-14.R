
# Load libraries
library(data.table)
library(socialmixr)
library(ggplot2)
library(patchwork)

source("~/bayes-rate-consistency/paper/figure_scripts/figure-4.R")

# Wave 2
plot_empricial(2) / plot_socialmixr(2) / plot_gp(2) + plot_layout(nrow = 3)
ggsave("~/bayes-rate-consistency/paper/figures/sup-figure-11.jpeg", width = 19, height = 15.5, units = "cm")

# Wave 3
plot_empricial(3) / plot_socialmixr(3) / plot_gp(3) + plot_layout(nrow = 3)
ggsave("~/bayes-rate-consistency/paper/figures/sup-figure-12.jpeg", width = 19, height = 15.5, units = "cm")

# Wave 4
plot_empricial(4) / plot_socialmixr(4) / plot_gp(4) + plot_layout(nrow = 3)
ggsave("~/bayes-rate-consistency/paper/figures/sup-figure-13.jpeg", width = 19, height = 15.5, units = "cm")

# Wave 5
plot_empricial(5) / plot_socialmixr(5) / plot_gp(5) + plot_layout(nrow = 3)
ggsave("~/bayes-rate-consistency/paper/figures/sup-figure-14.jpeg", width = 19, height = 15.5, units = "cm")

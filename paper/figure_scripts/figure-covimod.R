library(data.table)
library(ggplot2)

dt_matrix_1 <- readRDS("~/bayes-rate-consistency-output/results/hsgp-m52-lrd_COVIMOD_multi_1234/intensity_matrix.rds")
dt_matrix_1[, comb := paste(gender, "to", alter_gender)]

dt_matrix_2 <- readRDS("~/bayes-rate-consistency-output/results/hsgp-m52-lrd_COVIMOD_multi_1527/intensity_matrix.rds")
dt_matrix_2[, comb := paste(gender, "to", alter_gender)]

dt_matrix_3 <- readRDS("~/bayes-rate-consistency-output/results/hsgp-m52-lrd_COVIMOD_multi_721/intensity_matrix.rds")
dt_matrix_3[, comb := paste(gender, "to", alter_gender)]

plot_matrix <- function(data, seed){
  ggplot(data[wave == 1], aes(age, alter_age)) +
    geom_tile(aes(fill = intensity_M)) +
    viridis::scale_fill_viridis(option = "H", limits = c(0, 0.25)) +
    coord_equal(expand = 0) +
    labs(x = "Age of contacting individuals", y = "Age of contacts",
         fill = "intensity", title = paste("Seed:", seed)) +
    facet_grid(~comb) +
    theme_bw() +
    guides(fill = guide_colourbar(barwidth = 0.8)) +
    theme(
      axis.text = element_text(size = 8),
      axis.title.x = element_text(size = 8),
      axis.title.y = element_text(size = 8),
      strip.background = element_blank(),
      strip.text = element_blank(),
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 8),
      legend.margin = margin(l = -0.2, unit = "cm"),
      plot.margin = margin(),
      plot.title = element_text(size = 8)
    )
}

plot_matrix(dt_matrix_1, 1) +
  plot_matrix(dt_matrix_2, 2) +
  plot_matrix(dt_matrix_3, 3) +
  plot_layout(nrow = 3, guides = "collect")

ggsave("~/bayes-rate-consistency/paper/figures/covimod-longitudinal-matrices-diff-seed.jpeg", width = 19, height = 15.5, units = "cm")


dt_marginal_1 <- readRDS("~/bayes-rate-consistency-output/results/hsgp-m52-lrd_COVIMOD_multi_1234/intensity_marginal_a.rds")
dt_marginal_2 <- readRDS("~/bayes-rate-consistency-output/results/hsgp-m52-lrd_COVIMOD_multi_1527/intensity_marginal_a.rds")
dt_marginal_3 <- readRDS("~/bayes-rate-consistency-output/results/hsgp-m52-lrd_COVIMOD_multi_721/intensity_marginal_a.rds")

plot_marginal <- function(data, seed){
  ggplot(data, aes(age, intensity_M)) +
    geom_ribbon(aes(ymin = intensity_CL, ymax = intensity_CU, fill = factor(wave)), alpha = 0.3) +
    geom_line(aes(color = factor(wave), linetype = "COVIMOD"), show.legend = FALSE) +
    scale_x_continuous(expand = c(0,0)) +
    scale_color_manual(values = c("#264653", "#2A9D8F", "#E9C46A", "#F4A261", "#E76F51")) +
    scale_fill_manual(values = c("#264653", "#2A9D8F", "#E9C46A", "#F4A261", "#E76F51")) +
    labs(x = "Age of contacting individuals", y = "Contact intensities", title = paste("Seed:", seed)) +
    facet_grid(gender ~ paste("Wave", wave)) +
    theme_bw() +
    guides(fill = "none") +
    theme(
      strip.background = element_blank(),
      strip.text = element_text(size = 8),
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 8),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 8),
      legend.margin = margin(t = -0.2, unit = 'cm'),
      plot.title = element_text(size = 8)
    )
}

plot_marginal(dt_marginal_1, 1) +
  plot_marginal(dt_marginal_2, 2) +
  plot_marginal(dt_marginal_3, 3) +
  plot_layout(nrow = 3, guides = "collect")

ggsave("~/bayes-rate-consistency/paper/figures/covimod-longitudinal-marginal-diff-seed.jpeg", units = "cm", width = 18, height = 24, dpi = 300)

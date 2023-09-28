library(data.table)
library(ggplot2)
library(viridis)
library(ggsci)

# Load data
data_polymod <- readRDS("~/bayes-rate-consistency/data/POLYMOD/polymod-single-age.rds")
dt_contacts <- data_polymod$contacts
dt_matrix <- readRDS("~/bayes-rate-consistency-output/results/hsgp-m52-rd-polymod-single-age/intensity_matrix.rds")
dt_marginal <- readRDS("~/bayes-rate-consistency-output/results/hsgp-m52-rd-polymod-single-age/intensity_marginal.rds")
dt_matrix_stratified <- readRDS("~/bayes-rate-consistency-output/results/hsgp-m52-rd-polymod-stratified-age/intensity_matrix.rds")

# Calculate MAE
dt_mae_single <- merge(dt_contacts, dt_matrix,
                       by = c("age", "gender", "alter_age", "alter_gender"),
                       all.x = TRUE)
dt_mae_single[, error := abs(m - intensity_M)]
print(mean(dt_mae_single$m))
print(mean(dt_mae_single$error))

dt_mae_stratified <- merge(dt_contacts, dt_matrix_stratified,
                           by = c("age", "gender", "alter_age", "alter_gender"),
                           all.x = TRUE)
dt_mae_stratified[, error := abs(m - intensity_M)]
print(mean(dt_mae_stratified$error))

dt_contacts[, comb := paste(gender, "to", alter_gender)]
dt_matrix[, comb := paste(gender, "to", alter_gender)]
dt_matrix_stratified[, comb := paste(gender, "to", alter_gender)]

plt_empirical <- ggplot(dt_contacts[m <= 4.0], aes(age, alter_age)) +
  geom_tile(aes(fill = m)) +
  scale_fill_viridis(option = "H") +
  coord_equal(expand = FALSE) +
  labs(x = "Age of contacting individuals",
       y = "Age of contacts",
       fill = "Intensity") +
  facet_grid(~comb) +
  theme_bw() +
  guides(fill = guide_colourbar(barwidth = 0.8)) +
  theme(
    aspect.ratio = 1,
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 7),
    axis.title = element_text(size = 8),
    axis.title.x = element_blank(),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.margin = margin(l = -0.2, unit = "cm"),
    strip.background = element_blank(),
    strip.text = element_text(size = 8),
    plot.margin = margin()
  )

plt_single <- ggplot(dt_matrix, aes(x = age, y = alter_age)) +
  geom_tile(aes(fill = intensity_M)) +
  viridis::scale_fill_viridis(option = "H", limits = c(0, 4.0)) +
  coord_equal(expand = 0) +
  labs(x = "Age of contacting individuals", y = "Age of contacts", fill = "Intensity") +
  facet_grid(~comb) +
  theme_bw() +
  guides(fill = guide_colourbar(barwidth = 0.8)) +
  theme(
    axis.text = element_text(size = 8),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 8),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank(),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.margin = margin(l = -0.2, unit = "cm"),
    plot.margin = margin()
  )

plt_stratified <- ggplot(dt_matrix_stratified, aes(x = age, y = alter_age)) +
  geom_tile(aes(fill = intensity_M)) +
  viridis::scale_fill_viridis(option = "H", limits = c(0, 4.0)) +
  coord_equal(expand = 0) +
  labs(x = "Age of contacting individuals", y = "Age of contacts", fill = "Intensity") +
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
    plot.margin = margin()
  )

plt_empirical / plt_single / plt_stratified + plot_layout(nrow = 3, guides = "collect")
ggsave("~/bayes-rate-consistency/paper/figures/figure-4.tiff",
       device = "tiff", width = 19, height = 15.5, units = "cm")

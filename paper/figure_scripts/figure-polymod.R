library(data.table)
library(ggplot2)
library(viridis)
library(ggsci)

dt_matrix <- readRDS("~/bayes-rate-consistency-output/results/hsgp-m52-rd-polymod-single-age/intensity_matrix.rds")
dt_marginal <- readRDS("~/bayes-rate-consistency-output/results/hsgp-m52-rd-polymod-single-age/intensity_marginal.rds")

ggplot(dt_matrix, aes(age, alter_age)) +
  geom_tile(aes(fill = intensity_M)) +
  scale_fill_viridis(option = "H") +
  coord_equal(expand = FALSE) +
  labs(x = "Age of contacting individuals",
       y = "Age of contacts",
       fill = "Intensity") +
  facet_grid(gender~alter_gender) +
  theme_bw() +
  theme(
    strip.background = element_blank()
  )

ggplot(dt_marginal, aes(age, intensity_M)) +
  geom_line(aes(color = gender)) +
  geom_ribbon(aes(ymin = intensity_CL, ymax = intensity_CU, fill = gender), alpha = 0.3) +
  ggsci::scale_color_nejm() +
  ggsci::scale_fill_nejm() +
  scale_x_continuous(expand = c(0, 0)) +
  theme_bw()

dt_matrix_stratified <- readRDS("~/bayes-rate-consistency-output/results/hsgp-m52-rd-polymod-stratified-age/intensity_matrix.rds")
dt_marginal_stratified <- readRDS("~/bayes-rate-consistency-output/results/hsgp-m52-rd-polymod-stratified-age/intensity_marginal.rds")

ggplot(dt_matrix_stratified, aes(age, alter_age)) +
  geom_tile(aes(fill = intensity_M)) +
  scale_fill_viridis(option = "H") +
  coord_equal(expand = FALSE) +
  labs(x = "Age of contacting individuals",
       y = "Age of contacts",
       fill = "Intensity") +
  facet_grid(gender~alter_gender) +
  theme_bw() +
  theme(
    strip.background = element_blank()
  )

ggplot(dt_marginal_stratified, aes(age, intensity_M)) +
  geom_line(aes(color = gender)) +
  geom_ribbon(aes(ymin = intensity_CL, ymax = intensity_CU, fill = gender), alpha = 0.3) +
  ggsci::scale_color_nejm() +
  ggsci::scale_fill_nejm() +
  scale_x_continuous(expand = c(0, 0)) +
  theme_bw()

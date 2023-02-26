library(data.table)
library(ggplot2)
library(patchwork)
library(ggsci)

COVID <- "preCOVID"
N <- "250"

dt_intensities_truth <- readRDS(file.path("~/bayes-rate-consistency",
                                          "data",
                                          "simulations",
                                          "intensity",
                                          COVID,
                                          "data.rds"))

dt_sim_data <- readRDS(file.path("~/bayes-rate-consistency-output",
                                 "data",
                                 "simulations",
                                 "datasets",
                                 paste(COVID, N, "COVIMOD", sep = "_"),
                                 "data_1.rds"))

dt_intensities <- readRDS(file.path("~/bayes-rate-consistency-output",
                                    "results",
                                    paste(COVID, N, "COVIMOD", sep = "_"),
                                    "hsgp-m52-rd-30-20-1",
                                    "intensity_matrix.rds"))

dt_marginal <- readRDS(file.path("~/bayes-rate-consistency-output",
                                 "results",
                                 paste(COVID, N, "COVIMOD", sep = "_"),
                                 "hsgp-m52-rd-30-20-1",
                                 "intensity_marginal.rds"))

# Plot true contact intensity matrix
dt_intensities_truth_mm <- dt_intensities_truth[gender == "Male" & alter_gender == "Male"]
plt_intensities_truth <- ggplot(dt_intensities_truth_mm, aes(age, alter_age)) +
  geom_tile(aes(fill = cntct_intensity)) +
  coord_equal() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  viridis::scale_fill_viridis(option="H", limits=c(0,2)) +
  labs(x = "Age of contacting individuals",
       y = "Age of contacts",
       fill = "Intensity",
       title = "True intensities\nMale-Male") +
  theme_bw() +
  guides(fill = guide_colourbar(barwidth = 0.8)) +
  theme(
    plot.title = element_text(face = "bold", size = 8),
    axis.text = element_text(size = 8),
    # axis.title.x = element_text(size = 8),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 8),
    legend.title = element_text(size = 8, vjust = 0.85),
    legend.text = element_text(size = 8),
    legend.margin = margin(l = -0.1)
  )


# Plot simulated data
dt_sim_data_mm <- dt_sim_data[gender == "Male" & alter_gender == "Male"]
dt_sim_data_mm <- unique(dt_sim_data_mm[,.(age, gender, alter_age_strata, alter_gender, y_strata)])
plt_sim_data <- ggplot(dt_sim_data_mm, aes(age, alter_age_strata)) +
  geom_tile(aes(fill = y_strata)) +
  scale_y_discrete(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  viridis::scale_fill_viridis(option = "F", limits = c(0, 70)) +
  labs(x = "Age of contacting individuals", y="Age category of contacts", fill = "Count",
       title = "Simulated counts\nMale-Male") +
  theme_bw() +
  guides(fill = guide_colourbar(barwidth = 0.8)) +
  theme(
    aspect.ratio = 1,
    plot.title = element_text(face = "bold", size = 8),
    axis.text = element_text(size = 8),
    # axis.title.x = element_text(size = 8),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_text(size = 8, vjust = 0.85),
    legend.text = element_text(size = 8),
    legend.margin = margin(l = -2)
  )

# Plot recovered intensity matrix
dt_intensities_mm <- dt_intensities[gender == "Male" & alter_gender == "Male"]
plt_intensities_estimate <- ggplot(dt_intensities_mm, aes(age, alter_age)) +
  geom_tile(aes(fill = intensity_M)) +
  coord_equal() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  viridis::scale_fill_viridis(option="H", limits=c(0,2)) +
  labs(x = "Age of contacting individuals",
       y = "Age of contacts",
       fill = "Intensity",
       title = "Estimated intensities\nMale-Male") +
  theme_bw() +
  guides(fill = guide_colourbar(barwidth = 0.8)) +
  theme(
    plot.title = element_text(face = "bold", size = 8),
    axis.text = element_text(size = 8),
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    legend.title = element_text(size = 8, vjust = 0.85),
    legend.text = element_text(size = 8),
    legend.margin = margin()
  )

# Plot true and estimated marginal intensity
dt_marginal_truth <- dt_intensities_truth[, .(intensity = sum(cntct_intensity)),
                                          by = .(age, gender)]
plt_marginal_estimate <- ggplot(dt_marginal, aes(age, intensity_M)) +
  geom_ribbon(aes(ymin = intensity_CL, ymax = intensity_CU, fill = gender), alpha = 0.3) +
  geom_step(data = dt_marginal_truth, aes(y = intensity, color = gender, linetype = "True")) +
  geom_line(aes(y = intensity_M, color = gender, linetype = "Estimate")) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, NA)) +
  ggsci::scale_fill_nejm() +
  ggsci::scale_color_nejm() +
  labs(x = "Age of contacting individuals",
       y = "Contact intensity",
       title = "True and estimated\nmarginal intensities",
       linetype = NULL,
       color = NULL,
       fill = NULL) +
  theme_bw() +
  theme(
    aspect.ratio = 1,
    plot.title = element_text(face = "bold", size = 8),
    axis.text = element_text(size = 8),
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    legend.title = element_text(size = 8, vjust = 0.85),
    legend.text = element_text(size = 8),
    legend.margin = margin()
  )

(plt_intensities_truth + plt_sim_data + plt_intensities_estimate + plt_marginal_estimate) +
  plot_annotation(
    subtitle = paste(
      "Configuration: ",
      paste(COVID, N, "Difference-in-age", sep = ", ")
    )
  ) +
  plot_layout(nrow = 2) & theme(legend.position = "right",
                                plot.subtitle = element_text(size = 9, face = "bold"))

ggsave(file.path("~/bayes-rate-consistency/paper/figures",
                 paste0(
                   paste("sup-fig", COVID, N, sep = "-"),
                   ".png"
                 )),
       units = "cm",
       width = 13.37,
       height = 11,
       dpi = 300)


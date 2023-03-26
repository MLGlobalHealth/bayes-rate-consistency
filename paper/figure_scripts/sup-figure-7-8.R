library(data.table)
library(purrr)
library(ggplot2)
library(patchwork)
library(ggsci)

COVID <- "preCOVID"

sim_data_list <- map(c("250", "500", "1000", "2000"), ~{
  dt <- readRDS(file.path("~/bayes-rate-consistency-output",
                    "data",
                    "simulations",
                    "datasets",
                    paste(COVID, .x, "COVIMOD", sep = "_"),
                    "data_1.rds"))
  dt$sample_size <- paste("N =", .x)
  return(dt)
})

dt_sim_data <- rbindlist(sim_data_list)
dt_sim_data$sample_size <- factor(dt_sim_data$sample_size,
                                  levels = c("N = 250", "N = 500", "N = 1000", "N = 2000", "N = 5000"))

# Plot simulated data
dt_sim_data_mm <- dt_sim_data[gender == "Male" & alter_gender == "Male"]
dt_sim_data_mm <- unique(dt_sim_data_mm[,.(age, gender, alter_age_strata, alter_gender, y_strata, sample_size)])
plt_sim_data <- ggplot(dt_sim_data_mm, aes(age, alter_age_strata)) +
  geom_tile(aes(fill = y_strata)) +
  scale_y_discrete(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  viridis::scale_fill_viridis(option = "F", limits = c(0, NA)) +
  facet_grid(~sample_size) +
  labs(x = "Age of contacting individuals", y="Age of contacts", fill = "Count") +
  theme_bw() +
  guides(fill = guide_colourbar(barwidth = 0.8)) +
  theme(
    aspect.ratio = 1,
    plot.title = element_text(face = "bold", size = 8),
    axis.text = element_text(size = 8),
    strip.background = element_blank(),
    strip.text = element_text(size = 8),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 8),
    legend.title = element_text(size = 8, vjust = 0.85),
    legend.text = element_text(size = 8),
    legend.margin = margin(l = -0.3, unit = "cm")
  )

dt_intensity_list <- map(c("250", "500", "1000", "2000"), ~{
  dt <- readRDS(file.path("~/bayes-rate-consistency-output",
                          "results",
                          paste(COVID, .x, "COVIMOD", sep = "_"),
                          "hsgp-m52-rd-30-20-1",
                          "intensity_matrix.rds"))
  dt$sample_size <- paste("N =", .x)
  return(dt)
})

dt_intensity <- rbindlist(dt_intensity_list)
dt_intensity$sample_size <- factor(dt_intensity$sample_size,
                                   levels = c("N = 250", "N = 500", "N = 1000", "N = 2000", "N = 5000"))


# Plot recovered intensity matrix
dt_intensity <- dt_intensity[gender == "Male" & alter_gender == "Male"]
plt_intensities_estimate <- ggplot(dt_intensity, aes(age, alter_age)) +
  geom_tile(aes(fill = intensity_M)) +
  coord_equal() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  viridis::scale_fill_viridis(option="H", limits=c(0,2)) +
  facet_grid(~sample_size) +
  labs(x = "Age of contacting individuals",
       y = "Age of contacts",
       fill = "Intensity") +
  theme_bw() +
  guides(fill = guide_colourbar(barwidth = 0.8)) +
  theme(
    plot.title = element_text(face = "bold", size = 8),
    axis.text = element_text(size = 8),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 8),
    strip.background = element_blank(),
    strip.text = element_blank(),
    legend.title = element_text(size = 8, vjust = 0.85),
    legend.text = element_text(size = 8),
    legend.margin = margin(l = -0.3, unit = "cm")
  )

dt_intensities_truth <- readRDS(file.path("~/bayes-rate-consistency",
                              "data",
                              "simulations",
                              "intensity",
                              COVID,
                              "data.rds"))

dt_marginal_list <- map(c("250", "500", "1000", "2000"), ~{
  dt <- readRDS(file.path("~/bayes-rate-consistency-output",
                          "results",
                          paste(COVID, .x, "COVIMOD", sep = "_"),
                          "hsgp-m52-rd-30-20-1",
                          "intensity_marginal.rds"))
  dt$sample_size <- paste("N =", .x)
  return(dt)
})


# Plot true and estimated marginal intensity
dt_marginal_truth <- dt_intensities_truth[, .(intensity = sum(cntct_intensity)),
                                          by = .(age, gender)]
dt_marginal <- rbindlist(dt_marginal_list)
dt_marginal$sample_size <- factor(dt_marginal$sample_size,
                                  levels = c("N = 250", "N = 500", "N = 1000", "N = 2000", "N = 5000"))
plt_marginal_estimate <- ggplot(dt_marginal, aes(age, intensity_M)) +
  geom_ribbon(aes(ymin = intensity_CL, ymax = intensity_CU, fill = gender), alpha = 0.3) +
  geom_step(data = dt_marginal_truth, aes(y = intensity, color = gender, linetype = "True")) +
  geom_line(aes(y = intensity_M, color = gender, linetype = "Estimate")) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, NA)) +
  ggsci::scale_fill_nejm() +
  ggsci::scale_color_nejm() +
  facet_grid(~sample_size) +
  labs(x = "Age of contacting individuals",
       y = "Contact intensity",
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
    strip.background = element_blank(),
    strip.text = element_blank(),
    legend.title = element_text(size = 8, vjust = 0.85),
    legend.text = element_text(size = 8),
    legend.margin = margin(l = -0.2, unit = "cm")
  )

(plt_sim_data + plt_intensities_estimate + plt_marginal_estimate) + plot_layout(nrow = 3)

ggsave(file.path("~/bayes-rate-consistency/paper/figures",
                 paste0(
                   paste("sup-figure", "7", sep = "-"),
                   ".png"
                 )),
       units = "cm",
       width = 19,
       height = 15.5,
       dpi = 300)


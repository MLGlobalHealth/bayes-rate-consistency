library(data.table)
library(ggplot2)
library(ggsci)

repo_path <- "~/bayes-rate-consistency"

dt_longitudinal <- readRDS("~/bayes-rate-consistency-output/results/hsgp-m52-lrd_COVIMOD_multi_1234/intensity_marginal_a.rds")
dt_wave_4 <- readRDS("~/bayes-rate-consistency-output/results/hsgp-m52-rd-covimod-wave-4-norep/intensity_marginal.rds")

dt_wave_4$type <- "Wave 4 estimates from cross-sectional model\nwith repeating participants excluded from data"
dt_longitudinal_wave_4 <- dt_longitudinal[wave == 4]
dt_longitudinal_wave_4$type <- "Wave 4 estimates from longitudinal\nmodel adjusting for reporting fatigue"

dt <- rbind(dt_wave_4,
            dt_longitudinal_wave_4[, .(age, gender, intensity_CL, intensity_CU, intensity_M, type)])
dt$type <- factor(dt$type, levels = c("Wave 4 estimates from longitudinal\nmodel adjusting for reporting fatigue",
                                      "Wave 4 estimates from cross-sectional model\nwith repeating participants excluded from data"))

ggplot(dt, aes(age, intensity_M)) +
  geom_ribbon(aes(ymin = intensity_CL, ymax = intensity_CU, fill = type), alpha = 0.3) +
  geom_line(aes(color = type, linetype = type)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(values = c("#de425b", "#488f31")) +
  scale_fill_manual(values = c("#de425b", "#488f31")) +
  facet_grid(~gender) +
  labs(x = "Age of contacting individuals", y = "Contact intensity") +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 8),
    legend.margin = margin(t = -0.2, unit = "cm"),
    strip.background = element_blank(),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 8),
    strip.text = element_text(size = 8)
  )

ggsave(file.path(repo_path, "paper/figures", "sup-figure-16.jpeg"),
       units = "cm",
       width = 13.37,
       height = 8,
       dpi = 300)

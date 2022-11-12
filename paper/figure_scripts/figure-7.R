library(data.table)
library(ggplot2)

dt.covimod <- readRDS("~/Imperial/covimod-gp/results/hsgp-m52-lrd-5/intensity_marginal_a.rds")
dt.polymod <- readRDS("~/Imperial/covimod-gp/results/polymod-hsgp-m52-rd/intensity_marginal.rds")

dt.covimod$wave <- as.factor(paste("Wave", dt.covimod$wave))

ggplot(dt.covimod, aes(age, intensity_M)) +
  geom_ribbon(data = dt.polymod, aes(ymin = intensity_CL, ymax = intensity_CU), alpha = 0.2) +
  geom_line(data = dt.polymod, aes(linetype = "POLYMOD")) +
  geom_ribbon(aes(ymin = intensity_CL, ymax = intensity_CU, fill = wave), alpha = 0.3) +
  geom_line(aes(color = wave, linetype = "COVIMOD"), show.legend = FALSE) +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(values = c("#264653", "#2A9D8F", "#E9C46A", "#F4A261", "#E76F51")) +
  scale_fill_manual(values = c("#264653", "#2A9D8F", "#E9C46A", "#F4A261", "#E76F51")) +
  labs(x = "Age of contacting individuals", y = "Contact intensities") +
  facet_grid(gender ~ wave) +
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
    legend.margin = margin(t = -0.2, unit='cm')
  )

ggsave("~/Imperial/covimod-gp/paper/figures/figure-7.jpeg", units = "cm", width = 18, height = 12, dpi = 300)

library(data.table)
library(ggplot2)
library(patchwork)

dt <- readRDS("~/Imperial/covimod-gp/results/hsgp-m52-lrd-5/intensity_sliced.rds")

plt.sliced <- ggplot(dt[age %in% c(10, 20, 35, 70)], aes(alter_age, intensity_M)) +
  geom_ribbon(aes(ymin = intensity_CL, ymax = intensity_CU, fill = paste("Wave", wave)),
                  alpha = 0.5) +
  geom_line(aes(color = paste("Wave", wave)), size = 0.4) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(limits = c(0,NA)) +
  scale_color_manual(values = c("#264653", "#2A9D8F", "#E9C46A", "#F4A261", "#E76F51")) +
  scale_fill_manual(values = c("#264653", "#2A9D8F", "#E9C46A", "#F4A261", "#E76F51")) +
  labs(x = "Age of contacts", y = "Conditional contact intensity") +
  facet_grid(~paste("Age of contacting indiv:", age)) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.margin = margin(t = -0.2, unit = "cm"),
    legend.position = "bottom",
    strip.text = element_text(size = 8),
    strip.background = element_rect(color=NA, fill = "transparent"),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 8),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 8),
    panel.grid.minor.y = element_blank()
  )

dt[wave < 5, ratio_CL := NA]
dt[wave < 5, ratio_CU := NA]

# Plot relative increase from Wave 1
plt.ratio <- ggplot(dt[age %in% c(10, 20, 35, 70)], aes(alter_age, ratio_M)) +
  geom_ribbon(aes(ymin = ratio_CL, ymax = ratio_CU, fill = paste("Wave", wave)),
              alpha = 0.5) +
  geom_line(aes(color = paste("Wave", wave)), size = 0.4) +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(values = c("#264653", "#2A9D8F", "#E9C46A", "#F4A261", "#E76F51")) +
  scale_fill_manual(values = c("#264653", "#2A9D8F", "#E9C46A", "#F4A261", "#E76F51")) +
  labs(x = "Age of contacts", y = "Ratio relative to wave 1") +
  facet_grid(~paste("Age of contacting indiv.", age)) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.margin = margin(t = -0.2, unit = "cm"),
    legend.position = "bottom",
    strip.text = element_blank(),
    strip.background = element_rect(color=NA, fill = "transparent"),
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 8)
  )

plt.sliced / plt.ratio + plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave("paper/figures/figure-8.jpeg", units = "cm", width = 18, height = 12, dpi=300)


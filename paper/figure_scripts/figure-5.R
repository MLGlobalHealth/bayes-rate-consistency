library(data.table)
library(ggplot2)

dt <- readRDS("~/Imperial/covimod-gp/results/hsgp-m52-lrd-5/intensity_marginal_a.rds")
dt.noadj <- readRDS("~/Imperial/covimod-gp/results/hsgp-m52-lrd-noadj-5/intensity_marginal_a.rds")
dt.nogrp <- readRDS("~/Imperial/covimod-gp/results/hsgp-m52-lrd-nogroup-5/intensity_marginal_a.rds")
dt.norep <- readRDS("~/Imperial/covimod-gp/results/hsgp-m52-lrd-norep-5/intensity_marginal_a.rds")

dt$type <- "Adjusted"
dt.noadj$type <- "No adjustments"
dt.norep$type <- "Adjusted for missing & aggregate\ncontact reports but not reporting fatigue"
dt.nogrp$type <- "Adjusted for reporting fatigue but not\nmissing & aggregate contact reports"

# Remove CI bands
dt.noadj[, intensity_CL := NA]
dt.noadj[, intensity_CU := NA]

dt.nogrp[, intensity_CL := NA]
dt.nogrp[, intensity_CU := NA]

dt.norep[, intensity_CL := NA]
dt.norep[, intensity_CU := NA]

dt <- rbind(dt, dt.noadj, dt.norep, dt.nogrp)
dt$type <- factor(dt$type, levels = c("Adjusted", "Adjusted for missing & aggregate\ncontact reports but not reporting fatigue",
                                      "Adjusted for reporting fatigue but not\nmissing & aggregate contact reports", "No adjustments"))


ggplot(dt, aes(age, intensity_M)) +
  geom_ribbon(aes(ymin = intensity_CL, ymax = intensity_CU, group = type, fill = type), alpha = 0.3) +
  geom_line(aes(linetype = type, color = type)) +
  scale_x_continuous(expand = c(0,0)) +
  ggsci::scale_color_npg() +
  ggsci::scale_fill_npg() +
  labs(x = "Age of contacting individuals", y = "Contact intensities") +
  facet_grid(gender~paste("Wave", wave)) +
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

ggsave("~/Imperial/covimod-gp/paper/figures/figure-5.jpeg",
       units = "cm", width = 18, height = 12, dpi = 300)



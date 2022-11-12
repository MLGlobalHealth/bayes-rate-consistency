library(tidyverse)
library(data.table)
library(ggplot2)

# Load data
repo.path <- "~/Imperial/covimod-gp"
dt <- readRDS(file.path(repo.path, "results/hsgp-m52-lrd-5", "intensity_marginal_a.rds"))

ggplot(dt[wave > 1], aes(age, relchng_M)) +
  geom_line(aes(color = gender)) +
  geom_ribbon(aes(ymin = relchng_CL, ymax = relchng_CU, fill = gender), alpha = 0.5) +
  ggsci::scale_fill_nejm() +
  ggsci::scale_color_nejm() +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = "Age of contacting individuals", y = "Relative % change from wave 1",
       color = "Gender", fill = "Gender") +
  facet_grid(~paste("Wave", wave)) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.margin = margin(t = -2, unit = "pt"),
    strip.background = element_blank(),
    strip.text = element_text(size = 8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8)
  )

ggsave("~/Imperial/covimod-gp/paper/figures/figure-6.jpeg", units = "cm", width = 18, height = 9, dpi = 300)

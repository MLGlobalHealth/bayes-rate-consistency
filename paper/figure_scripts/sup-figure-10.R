library(data.table)
library(ggplot2)

dt.covimod <- readRDS("~/Imperial/covimod-gp/results/hsgp-m52-lrd-5/intensity_marginal_a.rds")
dt.covimod <- dt.covimod[, .(wave, age, ratioMF_CL, ratioMF_CU, ratioMF_M)]
dt.covimod <- unique( dt.covimod )
dt.covimod[, wave := paste("Wave", wave)]

dt.polymod <- readRDS("~/Imperial/covimod-gp/results/polymod-hsgp-m52-rd/intensity_marginal.rds")
dt.polymod <- unique( dt.polymod[, !c("gender", "intensity_CL", "intensity_CU", "intensity_M")] )
dt.polymod$wave <- "POLYMOD"

dt <- rbind(dt.polymod, dt.covimod)
dt$wave <- factor(dt$wave, levels = c("POLYMOD", "Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5"))

ggplot(dt, aes(age, ratioMF_M)) +
  geom_hline(aes(yintercept = 1.0), linetype = "dashed") +
  geom_ribbon(aes(ymin = ratioMF_CL, ymax = ratioMF_CU), alpha = 0.3, fill = "#AF0171") +
  geom_line(color = "#AF0171") +
  labs(x = "Age of contacting individuals", y = "Ratio of female to male contact intensity") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 2, 0.2)) +
  facet_grid(~wave) +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 8),
    legend.position = "bottom",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.margin = margin(t = -0.2, unit='cm')
  )

ggsave("~/Imperial/covimod-gp/paper/figures/sup-figure-10.jpeg",
       units = "cm", width = 18, height = 7, dpi = 300)



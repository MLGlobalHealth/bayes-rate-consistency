
# Load libraries
library(data.table)
library(ggplot2)
library(patchwork)

# Load results
dt.pre.se <- readRDS("~/Imperial/covimod-gp/results/preCOVID_2000_COVIMOD/hsgp-eq-rd-40-20_1/intensity_matrix.rds") # Pre-COVID19
dt.pre.m52 <- readRDS("~/Imperial/covimod-gp/results/preCOVID_2000_COVIMOD/hsgp-m52-rd-40-20_1/intensity_matrix.rds") # Pre-COVID19
dt.pre.m32 <- readRDS("~/Imperial/covimod-gp/results/preCOVID_2000_COVIMOD/hsgp-m32-rd-40-20_1/intensity_matrix.rds") # Pre-COVID19

plot_simulation <- function(data, title = NULL){
  data[, comb := paste(gender, "to", alter_gender)]

  ggplot(data, aes(age, alter_age)) +
    geom_tile(aes(fill = intensity_M)) +
    coord_equal(expand = 0) +
    viridis::scale_fill_viridis(option = "H", limits = c(0,2)) +
    guides(fill = guide_colourbar(barwidth = 0.8)) +
    facet_grid(~comb) +
    labs(x = "Age of contacting individual", y = "Age of contact", fill = "Intensity",
         title = title) +
    theme_bw() +
    theme(
      plot.title = element_text(size = 10, margin = margin(0, 0, -10, 0)),
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 8),
      strip.text = element_text(size = 8),
      strip.background = element_blank(),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 8),
      legend.margin = margin(l = -0.2, unit = "cm"),
    )
}

plt.se  <- plot_simulation(dt.pre.se, title = "Pre-COVID19, N=2000, Squared exponential kernel")
plt.m52 <- plot_simulation(dt.pre.m52, title = "Pre-COVID19, N=2000, Matérn 5/2 kernel")
plt.m32 <- plot_simulation(dt.pre.m32, title = "Pre-COVID19, N=2000, Matérn 3/2 kernel")

plt.se / plt.m52 / plt.m32 + plot_layout(nrow = 3, guides = "collect")

ggsave("~/Imperial/covimod-gp/paper/figures/sup-figure-2.jpeg", width = 18, height = 18, units = "cm")

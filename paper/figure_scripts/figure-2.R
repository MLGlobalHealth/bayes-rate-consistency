library(data.table)
library(ggplot2)
library(ggpubr)
library(patchwork)

repo.path <- "~/Imperial/covimod-gp"

# Simulated data
dt.pre <- readRDS(file.path(repo.path, "data/simulations/datasets/preCOVID_2000_COVIMOD/data_1.rds"))
dt.classic.pre <- readRDS(file.path(repo.path, "results/preCOVID_2000_COVIMOD/hsgp-m52-cd-20-20_1/intensity_matrix.rds"))
dt.restruct.pre <- readRDS(file.path(repo.path, "results/preCOVID_2000_COVIMOD/hsgp-m52-rd-40-20_1/intensity_matrix.rds"))

# Plot only MM contacts
dt.pre <- dt.pre[gender == "Male" & alter_gender == "Male"]
dt.classic.pre <- dt.classic.pre[gender == "Male" & alter_gender == "Male"]
dt.restruct.pre <- dt.restruct.pre[gender == "Male" & alter_gender == "Male"]

######------------------- Pre-COVID19 --------------------######

# Plot
p11 <- ggplot(dt.pre, aes(age, alter_age)) +
  geom_tile(aes(fill = cntct_intensity)) +
  coord_equal() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  viridis::scale_fill_viridis(option="H", limits=c(0,2)) +
  labs(x = "Age of contacting individuals", y="Age of contacts", fill = "Intensity",
       title = "Simulated intensities") +
  theme_bw() +
  guides(fill = guide_colourbar(barwidth = 0.8)) +
  theme(
    plot.title = element_text(face = "bold", size = 8),
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 8),
    axis.title.x = element_blank(),
    legend.title = element_text(size = 8, vjust = 0.85),
    legend.text = element_text(size = 8),
    legend.margin = margin()
  )

dt.strata.pre <- unique(dt.pre[, .(age, alter_age_strata, part, y_strata)])
p12 <- ggplot(dt.pre, aes(age, alter_age_strata)) +
  geom_tile(aes(fill = y_strata)) +
  scale_y_discrete(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  viridis::scale_fill_viridis(option = "F", limits = c(0,200)) +
  labs(x = "Age of contacting individuals", y="Age category of contacts", fill = "Count",
       title = "Simulated counts") +
  theme_bw() +
  guides(fill = guide_colourbar(barwidth = 0.8)) +
  theme(
    aspect.ratio = 1,
    plot.title = element_text(face = "bold", size = 8),
    axis.text = element_text(size = 8),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_text(size = 8, vjust = 0.85),
    legend.text = element_text(size = 8),
    legend.margin = margin()
  )

p13 <- ggplot(dt.classic.pre, aes(age, alter_age)) +
  geom_tile(aes(fill = intensity_M)) +
  coord_equal() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  viridis::scale_fill_viridis(option="H", limits=c(0,2)) +
  labs(x = "Age of contacting individuals", y="Age of contacts", fill = "Intensity",
       title = "Age-age") +
  theme_bw() +
  guides(fill = guide_colourbar(barwidth = 0.8)) +
  theme(
    plot.title = element_text(face = "bold", size = 8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 8),
    legend.title = element_text(size = 8, vjust = 0.85),
    legend.text = element_text(size = 8),
    legend.margin = margin()
  )

dt.restruct.pre$type <- "Pre-COVID19"
p14 <- ggplot(dt.restruct.pre, aes(age, alter_age)) +
  geom_tile(aes(fill = intensity_M)) +
  coord_equal() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  viridis::scale_fill_viridis(option="H", limits=c(0,2)) +
  labs(x = "Age of contacting individuals", y="Age of contacts", fill = "Intensity", title = "Difference-in-age") +
  theme_bw() +
  guides(fill = guide_colourbar(barwidth = 0.8)) +
  theme(
    plot.title = element_text(face = "bold", size = 8),
    axis.text = element_text(size = 8),
    axis.title.x = element_text(size = 8),
    axis.title.y = element_blank(),
    legend.title = element_text(size = 8, vjust = 0.85),
    legend.text = element_text(size = 8),
    legend.margin = margin()
  )

p11 + p12 + p13 + p14 + plot_layout(nrow = 2, ncol = 2, widths = c(1,1), guides = "collect") & theme( legend.position = "right")

ggsave(file.path(repo.path, "paper/figures", "figure-2.jpeg"), units = "cm", width = 13.37, height = 12, dpi=300)



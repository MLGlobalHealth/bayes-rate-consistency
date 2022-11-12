library(data.table)
library(ggplot2)
library(ggpubr)
library(patchwork)

repo.path <- "~/Imperial/covimod-gp"

# Simulated data
dt.in <- readRDS(file.path(repo.path, "data/simulations/datasets/inCOVID_2000_COVIMOD/data_1.rds"))
dt.classic.in <- readRDS(file.path(repo.path, "results/inCOVID_2000_COVIMOD/hsgp-m52-cd-20-20_1/intensity_matrix.rds"))
dt.restruct.in <- readRDS(file.path(repo.path, "results/inCOVID_2000_COVIMOD/hsgp-m52-rd-40-20_1/intensity_matrix.rds"))

# Plot only MM contacts
dt.in <- dt.in[gender == "Male" & alter_gender == "Male"]
dt.classic.in <- dt.classic.in[gender == "Male" & alter_gender == "Male"]
dt.restruct.in <- dt.restruct.in[gender == "Male" & alter_gender == "Male"]

######------------------- In-COVID19 --------------------######

# Simulated intensities
p21 <- ggplot(dt.in, aes(age, alter_age)) +
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
    axis.text = element_text(size = 8),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 8),
    legend.title = element_text(size = 8, vjust = 0.85),
    legend.text = element_text(size = 8),
    legend.margin = margin()
  )

# Simulated counts
dt.strata.in <- unique(dt.in[, .(age, alter_age_strata, part, y_strata)])
p22 <- ggplot(dt.in, aes(age, alter_age_strata)) +
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
    axis.title = element_blank(),
    legend.title = element_text(size = 8, vjust = 0.85),
    legend.text = element_text(size = 8),
    legend.margin = margin()
  )

# Age-age
p23 <- ggplot(dt.classic.in, aes(age, alter_age)) +
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
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    legend.title = element_text(size = 8, vjust = 0.85),
    legend.text = element_text(size = 8),
    legend.margin = margin()
  )

# Difference-in-age
dt.restruct.in$type <- "In-COVID19"
p24 <- ggplot(dt.restruct.in, aes(age, alter_age)) +
  geom_tile(aes(fill = intensity_M)) +
  coord_equal() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  viridis::scale_fill_viridis(option="H", limits=c(0,2)) +
  labs(x = "Age of contacting individuals", y="Age of contacts", fill = "Intensity",
       title = "Difference-in-age") +
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

(p21 + p22 + p23 + p24) + plot_layout(nrow = 2, widths = c(1,1), guides = "collect") &  theme(legend.position = "right")

ggsave(file.path(repo.path, "paper/figures", "figure-3.jpeg"), units = "cm", width = 13.37, height = 12, dpi=300)


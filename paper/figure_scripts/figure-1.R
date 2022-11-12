library(data.table)
library(patchwork)
library(ggplot2)
library(zoo)
library(ggpubr)
source("~/Imperial/covimod-gp/R/covimod-utility.R")

theme_set(theme_bw())

# COVIMOD
covimod_survey <- load_covimod_data("~/Imperial/covimod-gp")
covimod_data <- readRDS("~/Imperial/covimod-gp/data/COVIMOD/COVIMOD-single.rds")

# Sample size
dt.part <- covimod_data$offsets
dt.part <- dt.part[, .(N = sum(N)), by=.(wave)]

dt.dates <- covimod_survey$part[wave <= 5]
dt.dates <- dt.dates[, .(start_date = min(date), end_date = max(date)), by=.(wave)]

dt.size <- merge(dt.part, dt.dates, by="wave")

# COVID cases
df.covid.cases <- readr::read_csv("~/Imperial/covimod-gp/data/time-series-covid19-confirmed-germany.csv")
dt.covid.cases <- as.data.table(df.covid.cases)
dt.covid.cases <- dt.covid.cases[date <= "2020-08-01"]

# COVID deaths
df.covid.deaths <- readr::read_csv("~/Imperial/covimod-gp/data/time-series-covid19-deaths-germany.csv")
dt.covid.deaths <- as.data.table(df.covid.deaths)
dt.covid.deaths <- dt.covid.deaths[date <= "2020-08-01"]

# Stringency index
df.covid.idx <- readr::read_csv("~/Imperial/covimod-gp/data/owid-covid-data.csv")
dt.covid.idx <- as.data.table(df.covid.idx)
dt.covid.idx <- dt.covid.idx[location == "Germany" & date <= "2020-08-01"]

##### Cumulative deaths #####
p_death <- ggplot(dt.covid.deaths, aes(date, cum_deaths)) +
  geom_line(aes(color = "Cumulative deaths"), size = 0.7) +
  geom_line(data = dt.covid.idx, aes(x = date, y = stringency_index*100, color = "Stringency index")) +
  geom_ribbon(aes(ymin=0, ymax=cum_deaths), fill="#E84A5F", alpha=0.4) +
  scale_color_manual(values = c("#de425b", "#004c6d")) +
  scale_y_continuous(
    limits = c(0,10000),
    sec.axis = sec_axis(~.*.01, name = "Stringency index [%]")
  ) +
  geom_rect(data = dt.size, aes(xmin = start_date, xmax = end_date, ymin = 0, ymax = Inf),
            inherit.aes = FALSE, alpha = 0.3) +
  scale_x_date(date_breaks = "1 month", date_labels = "%B", expand = c(0,0)) +
  labs(x = "Date", y="Cumulative COVID-19 deaths [N]", color = "") +
  theme(
    axis.text.x = element_text(size = 8, hjust = 0.8),
    axis.text.y = element_text(size = 7),
    axis.title = element_text(size = 8),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    plot.margin = margin(r=5),
    legend.position = c(0.2, 0.92),
    legend.background = element_blank(),
    legend.text = element_text(size = 7)
  )

##### Daily cases #####
p_cases <- ggplot(dt.covid.cases, aes(x = date)) +
  geom_bar(aes(y=daily_cases), stat="identity", fill="#E84A5F", alpha=0.4) +
  geom_line(aes(y=seven_day_avg, color="Daily COVID-19 cases"),  size = 0.7) +
  geom_line(data = dt.covid.idx, aes(x = date, y = stringency_index*80, color = "Stringency index")) +
  scale_color_manual(values = c("#de425b", "#004c6d")) +
  scale_y_continuous(
    limits = c(0,8000),
    sec.axis = sec_axis(~.*.0125, name = "Stringency index [%]")
  ) +
  geom_rect(data = dt.size, aes(xmin = start_date, xmax = end_date, ymin = 0, ymax = Inf),
            inherit.aes = FALSE, alpha = 0.3) +
  geom_text(data = dt.size, aes(x = start_date + floor((end_date - start_date)/2), y = 7000, label = paste("Wave", wave)),
            hjust = "center", vjust = "center", size = 2.2, inherit.aes = FALSE, angle=90) +
  scale_x_date(date_breaks = "1 month", date_labels = "%B", expand = c(0,0)) +
  labs(x = "Date", y="Daily COVID-19 cases [N]", color = "") +
  theme(
    axis.text.y = element_text(size = 7),
    axis.title = element_text(size = 8),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    plot.margin = margin(r=5),
    legend.position = c(0.2, 0.92),
    legend.background = element_blank(),
    legend.text = element_text(size = 7)
  )

##### Sample size #####
covimod_data <- readRDS("~/Imperial/covimod-gp/data/COVIMOD/COVIMOD-multi.rds")
dt.part <- covimod_data$offsets
dt.N <- dt.part[, .(N = sum(N)), by=.(wave)]

dt.rep <- dt.part[, .(N = sum(N)), by=.(wave, rep)]
dt.rep[, P := N/sum(N), by=.(wave)]
dt.rep <- setorder(dt.rep, wave, -rep)

dt.rep[, pos := cumsum(N) - 0.5*N, by=.(wave)]

p_sample <- ggplot(dt.rep, aes(wave, N)) +
  geom_bar(aes(fill = as.factor(rep)), stat = "identity", position="stack") +
  geom_label(aes(y = pos, label = paste0(round(P*100,1), "%")), size = 2,
             alpha=0, label.size = NA, color = "white") +
  geom_label(data = dt.N, aes(x = wave, y = N + 50, label = paste0("N=",N)),
             alpha=0,size = 2, inherit.aes = FALSE, label.size = NA) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2000)) +
  scale_fill_manual(values = c("#355C7D", "#6C5B7B", "#C06C84", "#F67280", "#F8B195")) +
  labs(x = "Wave", y = "Sample size", fill = "Repeat\nsurveillance") +
  theme(
    axis.title = element_text(size = 8),
    axis.text.y = element_text(size = 7),
    axis.text.x = element_text(size = 8),
    plot.margin = margin(),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.position = "right",
    legend.margin = margin(l=-1)
  )

##### Plotting #####
design <- "
  113
  223
"
patchwork <- p_cases + p_death + p_sample + plot_layout(design = design)
patchwork + plot_annotation(tag_levels = "A") & theme(plot.tag = element_text(size = 10))

ggsave("~/Imperial/covimod-gp/paper/figures/figure-1.jpeg",
       units = "cm", width = 19, height = 12, dpi = 300)



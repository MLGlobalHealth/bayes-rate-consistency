library(data.table)
library(socialmixr)
library(patchwork)
library(ggplot2)

source("~/Imperial/covimod-gp/R/covimod-utility.R")
source("~/Imperial/covimod-gp/R/stan-utility.R")

##### Empirical estimates #####
plot_empricial <- function(.wave, cap = 3){
  covimod_data <- readRDS("~/Imperial/covimod-gp/data/COVIMOD/COVIMOD-single.rds")
  dt.cnt <- covimod_data$contacts[wave == .wave]
  dt.off <- covimod_data$offsets[wave == .wave]

  dt.cnt <- dt.cnt[, !"wave"]
  dt.off <- dt.off[, !c("wave", "N", "zeta")]

  dt.cnt <- merge(make_grid(84, gender = TRUE), dt.cnt, by=c("age","gender","alter_age_strata","alter_gender"), all.x = TRUE)
  dt <- merge(dt.cnt, dt.off, by=c("age", "gender"), all.x = TRUE)

  dt[is.na(y) & !is.na(N), y := 0.0]
  dt[is.na(zeta) & !is.na(N), zeta := 1.0]

  dt[, comb := paste(gender, "to", alter_gender)]
  dt[, m := y / N * 1/zeta]
  dt[m > cap, m := cap]
  dt <- dt[!is.na(m)]

  ggplot(dt, aes(x = age, alter_age_strata)) +
    geom_tile(aes(fill = m)) +
    facet_grid(~comb) +
    scale_x_continuous(expand = c(0,0), limits = c(0,84)) +
    scale_y_discrete(expand = c(0,0)) +
    labs(x = "", y = "Age of contacts", fill = "Intensity") +
    viridis::scale_fill_viridis(option = "H") +
    theme_bw() +
    guides(fill = guide_colourbar(barwidth = 0.8)) +
    theme(
      aspect.ratio = 1,
      axis.text.x = element_text(size = 8),
      axis.text.y = element_text(size = 7),
      axis.title = element_text(size = 8),
      axis.title.x = element_blank(),
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 8),
      legend.margin = margin(l = -0.2, unit = "cm"),
      strip.background = element_blank(),
      strip.text = element_text(size = 8),
      plot.margin = margin()
    )
}

plt_empirical <- plot_empricial(.wave = 1)

##### Socialmixr #####
# Preprocess population data
process_pop_data <- function(x){
  d <- x
  d$lower.age.limit <- cut(d$age,  c(0, 5, 10, 15, 20, 25, 35, 45, 55, 65, 70, 75, 80, 85), right = F)
  d <- d[, .(population = sum(pop)), by = .(lower.age.limit)]
  d[is.na(lower.age.limit), lower.age.limit := "85"]
  d[, lower.age.limit := gsub("\\[([0-9]+),([0-9]+)\\)", "\\1", lower.age.limit)]
  d[, lower.age.limit := as.integer(lower.age.limit)]

  return(d)
}

preprocess_covimod <- function(.wave){
  covimod_data <- load_covimod_data("~/Imperial/covimod-gp")

  dt.part <- covimod_data$part
  dt.nhh <- covimod_data$nhh
  dt.hh <- covimod_data$hh

  # Impute child age
  dt.part <- impute_child_age(dt.part, seed=1527)
  setnames(dt.part, old = c("new_id", "imp_age"), new = c("part_id", "part_age"))
  dt.part$country <- "Germany"
  dt.part <- dt.part[age_strata != "85+"]

  # Combine household and non-household contacts
  setnames(dt.hh, "hh_met_this_day", "y")
  dt.hh$type <- "hh"
  dt.nhh$y <- 1
  dt.nhh$type <- "nhh"

  dt.cmb <- rbind(dt.nhh[,.(new_id, wave, type, alter_age_strata, alter_gender, y)],
                  dt.hh[, .(new_id, wave, type, alter_age_strata, alter_gender, y)])

  dt.cmb$type <- factor(dt.cmb$type, levels=c("hh", "nhh"))
  dt.cmb <- dt.cmb[order(wave, new_id),]
  dt.cmb <- dt.cmb[y > 0]
  dt.cmb[, cnt_age_est_min := gsub("([0-9]+)-([0-9]+)", "\\1", alter_age_strata)]
  dt.cmb[, cnt_age_est_max := gsub("([0-9]+)-([0-9]+)", "\\2", alter_age_strata)]

  suppressWarnings({
    dt.cmb[, cnt_age_est_min := as.integer(cnt_age_est_min)]
    dt.cmb[, cnt_age_est_max := as.integer(cnt_age_est_max)]
  })
  setnames(dt.cmb, old = c("new_id", "alter_gender"), new = c("part_id", "cnt_gender"))

  # Exclude contacts age 85 and above
  dt.cmb <- dt.cmb[alter_age_strata != "85+"]

  # Pre-process population data
  dt.pop <- process_pop_data(covimod_data$pop)

  list(cmb = dt.cmb[wave == .wave], part = dt.part[wave == .wave], pop = dt.pop)
}

plot_socialmixr <- function(.wave){

  # Pre-process
  d <- preprocess_covimod(.wave)

  # Unpack
  cmb <-  d$cmb
  part <- d$part
  pop <-  d$pop

  # Make survey objects
  covimod_surveys <- list(
    survey(participants = part[gender == "Male"],   contacts = cmb[cnt_gender == "Male"]),
    survey(participants = part[gender == "Female"], contacts = cmb[cnt_gender == "Female"]),
    survey(participants = part[gender == "Male"],   contacts = cmb[cnt_gender == "Female"]),
    survey(participants = part[gender == "Female"], contacts = cmb[cnt_gender == "Male"])
  )

  contact_matrices <- purrr::map(covimod_surveys, ~{
    age_limits <- c(0, 5, 10, 15, 20, 25, 35, 45, 55, 65, 70, 75, 80)
    if(nrow(.x$participants[age_strata == "80-84"]) == 0){ # If there are no participants older than 80
      age_limits <- c(0, 5, 10, 15, 20, 25, 35, 45, 55, 65, 70, 75)
    }

    contact_matrix(.x,
                   age.limits = age_limits,
                   survey.pop = pop,
                   counts = FALSE,
                   symmetric = TRUE,
                   missing.participant.age = "remove",
                   missing.contact.age = "sample")
  })

  age_strata <- c("0-4","5-9","10-14","15-19","20-24","25-34","35-44",
                  "45-54","55-64","65-69","70-74","75-79","80-84")

  contact_matrix_table <- function(x){
    tmp <- contact_matrices[[x]]$matrix
    df <- reshape2::melt(tmp, varnames = c("age", "alter_age"), value.name = "contacts", id.vars = NULL)
    dt <- as.data.table(df)
    dt <- dt[!is.na(age)]
    dt[, age := age_strata[age]]
    dt[, alter_age := age_strata[as.numeric(alter_age)]]

    dt[, comb := x]
  }

  data_list <- purrr::map(1:length(contact_matrices), ~contact_matrix_table(.x))
  dt <- rbindlist(data_list)
  dt$age <- factor(dt$age, levels = age_strata)
  dt$alter_age <- factor(dt$alter_age, levels = age_strata)
  dt[,comb := fcase(comb == 1, "Male to Male",
                    comb == 2, "Female to Female",
                    comb == 3, "Male to Female",
                    comb == 4, "Female to Male")]

  # Plot
  ggplot(dt, aes(x = age, y = alter_age)) +
    geom_tile(aes(fill = contacts)) +
    viridis::scale_fill_viridis(option = "H", limits = c(0, 1.25)) +
    scale_y_discrete(expand = c(0,0)) +
    scale_x_discrete(expand = c(0,0)) +
    labs(x = "", y = "Age of contacts", fill = "Intensity") +
    facet_wrap(~comb, ncol = 4, nrow = 1) +
    theme_bw() +
    guides(fill = guide_colourbar(barwidth = 0.8)) +
    theme(
      aspect.ratio = 1,
      axis.text.x = element_text(size = 7, vjust = 0.7, angle = 45),
      axis.text.y = element_text(size = 7),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 8),
      strip.background = element_blank(),
      strip.text = element_blank(),
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 8),
      legend.margin = margin(l = -0.2, unit = "cm"),
      plot.margin = margin()
    )
}

plt_socialmixr <- plot_socialmixr(.wave = 1)

##### Estimated contact intensities #####
plot_gp <- function(.wave){
  dt <- readRDS("~/Imperial/covimod-gp/results/hsgp-m52-lrd-5/intensity_matrix.rds")
  dt <- dt[wave == .wave]
  dt[, comb := paste(gender, "to", alter_gender)]

  ggplot(dt, aes(x = age, y = alter_age)) +
    geom_tile(aes(fill = intensity_M)) +
    viridis::scale_fill_viridis(option = "H", limits = c(0, NA)) +
    coord_equal(expand = 0) +
    labs(x = "Age of contacting individuals", y = "Age of contacts", fill = "Intensity") +
    facet_grid(~comb) +
    theme_bw() +
    guides(fill = guide_colourbar(barwidth = 0.8)) +
    theme(
      axis.text = element_text(size = 8),
      axis.title.x = element_text(size = 8),
      axis.title.y = element_text(size = 8),
      strip.background = element_blank(),
      strip.text = element_blank(),
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 8),
      legend.margin = margin(l = -0.2, unit = "cm"),
      plot.margin = margin()
    )
}

plt_gp <- plot_gp(1)

plt_empirical / plt_socialmixr / plt_gp + plot_layout(nrow = 3)

ggsave("~/Imperial/covimod-gp/paper/figures/figure-4.jpeg", width = 19, height = 15.5, units = "cm")

library(tidyverse)
library(data.table)
library(ggplot2)
library(ggpubr)

source("~/Imperial/covimod-gp/R/covimod-utility.R")

# Load data
covimod <- load_covimod_data("~/Imperial/covimod-gp")
dt.part <- covimod$part
dt.nhh <- covimod$nhh
dt.hh <- covimod$hh

# Limit to first 5 waves
dt.part <- dt.part[wave <= 5]
dt.nhh <- dt.nhh[wave <= 5]
dt.hh <- dt.hh[wave <= 5]

## Participant data
# Remove participants with missing age-strata or gender information
nrow(dt.part[is.na(gender) | is.na(age_strata)])
dt.part <- dt.part[!is.na(gender) & !is.na(age_strata)]

# The number of times a participant repeatedly participated in the survey
dt.part[, rep := seq_len(.N), by=.(new_id)]
dt.part[, rep := rep - 1]
dt.part[, rep := ifelse(rep > 4, 4, rep)]

# Impute children age by sampling from uniform distribution
dt.part <- impute_child_age(dt.part, seed=1527)

## Non-household contacts
### Ambiguous contacts
# Identify contacts with missing age and gender
dt.amb <- dt.nhh[(is.na(alter_age_strata) | is.na(alter_gender))]

# Treat all as missing (some have either gender or age-strata info)
dt.amb <- dt.amb[, .(y_amb = .N), by=c("new_id", "wave")]
dt.amb <- merge(dt.amb, dt.part[, .(new_id, wave, gender, imp_age)], by=c("new_id", "wave"), all.x = TRUE)

# Remove ambiguous contacts from original nhh data
dt.nhh <- dt.nhh[!(is.na(alter_age_strata) | is.na(alter_gender))]

## Group Contacts
SDcols_Q75 <- c("Q75_u18_work", "Q75_u18_school", "Q75_u18_else",
                "Q75_1864_work", "Q75_1864_school", "Q75_1864_else",
                "Q75_o64_work", "Q75_o64_school", "Q75_o64_else")

SDcols_Q76 <- c("Q76_u18_work",  "Q76_u18_school",  "Q76_u18_else",
                "Q76_1864_work", "Q76_1864_school", "Q76_1864_else",
                "Q76_o64_work",  "Q76_o64_school",  "Q76_o64_else")

dt.part[, y_grp := rowSums(.SD, na.rm = T), .SDcols = SDcols_Q75]
dt.part[y_grp > 60, y_grp := 60]

dt.grp <- dt.part[, .(y = sum(y_grp)), by=.(wave, imp_age, gender)]
dt.grp$type <- "grp"

## Combine household and non-household data
setnames(dt.hh, "hh_met_this_day", "y")
dt.hh$type <- "hh"

dt.nhh$y <- 1
dt.nhh$type <- "nhh"

# Combine household and non-household contacts
dt.cmb <- rbind(dt.nhh[,.(new_id, wave, type, alter_age_strata, alter_gender, y)],
                dt.hh[,.(new_id, wave, type, alter_age_strata, alter_gender, y)])

dt.cmb$type <- factor(dt.cmb$type, levels=c("hh", "nhh"))
dt.cmb <- dt.cmb[order(wave, new_id),]

# Merge with participant data
dt.cmb <- merge(dt.cmb, dt.part, by=c("new_id", "wave"), all.x = TRUE)
dt.cmb <- dt.cmb[, .(y = sum(y, na.rm = T)), by = .(wave, type, imp_age, gender)]

dt.amb$type <- "grp"
setnames(dt.amb, "y_amb", "y")
dt.amb <- dt.amb[, .(y = sum(y, na.rm = T)), by=.(wave, type, imp_age, gender)]

dt.sum <- rbind(dt.cmb, dt.amb, dt.grp)

dt.sum <- dt.sum[, .(y = sum(y, na.rm=T)), by=.(wave, type, imp_age, gender)]
dt.sum$type2 <- case_when(dt.sum$type == "hh" ~ "Household",
                          dt.sum$type == "nhh" ~ "Non-household",
                          dt.sum$type == "grp" ~ "Aggregated")
dt.sum$type2 <- factor(dt.sum$type2, levels=rev(c("Household", "Non-household", "Aggregated")))

dt.sum.sum <- dt.sum[, .(y = sum(y, na.rm=T)), by =.(wave, gender)]
dt.sum.sum <- dt.sum.sum[!is.na(gender)]

ggplot(dt.sum[imp_age < 85], aes(imp_age, y)) +
  geom_bar(aes(fill = type2), stat="identity") +
  geom_text(data = dt.sum.sum, aes(x = 49, y = 300, label = paste("Observed contacts: ", y)),
             size = 2.7, hjust = 0) +
  scale_x_continuous(expand=c(0,0)) +
  scale_fill_manual(values = rev(c("#0D353F", "#017979", "#F57D6C"))) +
  labs(x = "Participant age", y = "Frequency", fill = "Contact type") +
  facet_grid(paste("Wave", wave) ~ gender) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.margin = margin(t = -2),
    strip.background = element_blank(),
    strip.text = element_text(size = 8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 8),
    panel.grid.minor = element_blank()
  )

ggsave("~/Imperial/covimod-gp/paper/figures/sup-figure-5.jpeg",
       units = "cm", width = 18, height = 18, dpi = 300)

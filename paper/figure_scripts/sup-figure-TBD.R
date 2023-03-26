# Load libraries
library(data.table)
library(ggplot2)
library(purrr)
library(boot)
library(patchwork)
library(ggpubr)

# Load helpers
source("./R/load_covimod_data.R")
source("./R/fill_missing_child_ages.R")
source("./R/age_gender_grid.R")

SEED <- 1234

covimod <- load_covimod_data("~/bayes-rate-consistency")
dt_participants <- covimod$part
dt_non_household <- covimod$nhh
dt_household <- covimod$hh

# Limit to first 5 waves
dt_participants <- dt_participants[wave <= 5]
dt_non_household <- dt_non_household[wave <= 5]
dt_household <- dt_household[wave <= 5]

# Remove participants with missing age-strata or gender information
dt_participants <- dt_participants[!is.na(gender) & !is.na(age_strata)]

# The number of times a participant repeatedly participated in the survey
dt_participants[, rep := seq_len(.N), by = .(new_id)]
dt_participants[, rep := rep - 1]
dt_participants[, rep := ifelse(rep > 4, 4, rep)]

# Impute children age by sampling from uniform distribution
dt_participants <- fill_missing_child_ages(dt_participants, seed = SEED)

# Create a new index U which is a combined index of wave and rep
dt_participants[, u := fcase(wave == 1, 1,
                             wave == 2, 2 + rep,
                             wave >= 3, 1/2*(wave - 1)*(wave) + rep + 1)]

dt_participants <- dt_participants[u != 11]

# Identify contacts with missing age and gender
dt_missing <- dt_non_household[is.na(alter_age_strata) | is.na(alter_gender)]

# Append participant info
dt_missing <- merge(dt_missing[,.(new_id, wave)], dt_participants, by = c("new_id", "wave"), all.x = TRUE)

# Treat all as missing (some have either gender or age-strata info)
dt_missing <- dt_missing[, .(y_missing = .N), by = c("new_id", "u")]



# Truncate at 30 (to remove extreme cases where people reported 30+ contacts)
dt_missing <- merge(dt_missing, dt_participants, by = c("new_id", "u"), all.x = TRUE)

# Remove ambiguous contacts from original nhh data
dt_non_household <- dt_non_household[!(is.na(alter_age_strata) | is.na(alter_gender))]

# Group contacts
SDcols_Q75 <- c("Q75_u18_work", "Q75_u18_school", "Q75_u18_else",
                "Q75_1864_work", "Q75_1864_school", "Q75_1864_else",
                "Q75_o64_work", "Q75_o64_school", "Q75_o64_else")

dt_participants[, y_agg := rowSums(.SD, na.rm = T), .SDcols = SDcols_Q75]
dt_participants[y_agg > 60, y_agg := 60]

dt_aggregate <- dt_participants[, .(y = sum(y_agg)), by = .(wave, new_id)]
dt_aggregate_sum <- dt_aggregate[y > 0]

# Summarize household contacts
dt_household_sum <- dt_household[, .(y = sum(hh_met_this_day)), by = .(wave, new_id)]

# Summarize non-household contacts
dt_non_household_sum <- dt_non_household[, .(y = .N), by = .(wave, new_id)]

# Include missing & aggregate contacts in non-household contacts
dt_missing_sum <- dt_missing[, .(y = sum(y_missing)), by = .(wave, new_id)]

dt_all_non_hh <- rbind(dt_non_household_sum, dt_missing_sum, dt_aggregate_sum)
dt_all_non_hh <- dt_all_non_hh[, .(y = sum(y)), by = .(wave, new_id)]

dt_all <- rbind(dt_household_sum, dt_non_household_sum, dt_missing_sum, dt_aggregate_sum)
dt_all_sum <- dt_all[, .(y = sum(y)), by = .(wave, new_id)]

# Function to obtain the mean of the data
boot_mean <- function(data, indicies){
  data_subset <- data[indicies,]
  return(mean(data_subset$y, na.rm = TRUE))
}

boot_estimates <- function(dt){
  dt_list <- map(1:5, ~ {
    boot_out <- boot(data = dt[wave == .x,],
                     statistic = boot_mean,
                     R = 1000)
    boot_ci <- boot.ci(boot_out, type = "basic")

    data.table(wave = .x,
               M = boot_ci$t0,
               CL = boot_ci$basic[,4],
               CU = boot_ci$basic[,5])
  })

  return(rbindlist(dt_list))
}

dt_household_boot <- boot_estimates(dt_household_sum)
dt_non_household_boot <- boot_estimates(dt_non_household_sum)
dt_all_non_household_boot <- boot_estimates(dt_all_non_hh)
dt_all_boot <- boot_estimates(dt_all_sum)

dt_household_boot$type <- "Household"
dt_non_household_boot$type <- "Non-household\n(without missing & aggregate)"
dt_all_non_household_boot$type <- "Non-household\n(with missing & aggregate)"
dt_all_boot$type <- "All"

dt_all_boot <- rbind(dt_household_boot,
                     dt_non_household_boot,
                     dt_all_non_household_boot,
                     dt_all_boot)
dt_all_boot$type <- factor(dt_all_boot$type, levels = c("Household",
                                                        "Non-household\n(without missing & aggregate)",
                                                        "Non-household\n(with missing & aggregate)",
                                                        "All"))

dt_household_w1_sum <- dt_household_sum[wave == 1]
dt_non_household_w1_sum <- dt_non_household_sum[wave == 1]
dt_all_non_household_w1_sum <- dt_all_non_hh[wave == 1]
dt_all_w1_sum <- dt_all_sum[wave == 1]

repeat_participant_id <- dt_participants[wave == 4 & rep > 0, new_id]
dt_household_w4_sum <- dt_household_sum[wave == 4 & !(new_id %in% repeat_participant_id)]
dt_non_household_w4_sum <- dt_non_household_sum[wave == 4 & !(new_id %in% repeat_participant_id)]
dt_all_non_household_w4_sum <- dt_all_non_hh[wave == 4 & !(new_id %in% repeat_participant_id)]
dt_all_w4_sum <- dt_all_sum[wave == 4 & !(new_id %in% repeat_participant_id)]

dt_boot_w1 <- map(list(dt_household_w1_sum,
                       dt_non_household_w1_sum,
                       dt_all_non_household_w1_sum,
                       dt_all_w1_sum), ~ {
    boot_out <- boot(data = .x,
                     statistic = boot_mean,
                     R = 1000)
    boot_ci <- boot.ci(boot_out, type = "basic")

    data.table(wave = 1,
               M = boot_ci$t0,
               CL = boot_ci$basic[,4],
               CU = boot_ci$basic[,5])
})
dt_boot_w1 <- rbindlist(dt_boot_w1)

dt_boot_w4 <- map(list(dt_household_w4_sum,
                       dt_non_household_w4_sum,
                       dt_all_non_household_w4_sum,
                       dt_all_w4_sum), ~ {
                         boot_out <- boot(data = .x,
                                          statistic = boot_mean,
                                          R = 1000)
                         boot_ci <- boot.ci(boot_out, type = "basic")

                         data.table(wave = 4,
                                    M = boot_ci$t0,
                                    CL = boot_ci$basic[,4],
                                    CU = boot_ci$basic[,5])
                       })
dt_boot_w4 <- rbindlist(dt_boot_w4)

dt_boot_w14 <- rbind(dt_boot_w1, dt_boot_w4)
dt_boot_w14$type <- c("Household", "Non-household\n(without missing & aggregate)", "Non-household\n(with missing & aggregate)", "All",
                      "Household", "Non-household\n(without missing & aggregate)", "Non-household\n(with missing & aggregate)", "All")

dt_boot_w14$type <- factor(dt_boot_w14$type, levels = c("Household",
                                                        "Non-household\n(without missing & aggregate)",
                                                        "Non-household\n(with missing & aggregate)",
                                                        "All"))

# Plot results
plt_w14 <- ggplot(dt_boot_w14, aes(x = ordered(wave), y = M)) +
  geom_point(aes(color = type),
             size = 2,
             position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(ymin = CL, ymax = CU, color = type),
                position = position_dodge(width = 0.4),
                width = 0.3) +
  scale_y_continuous(limits = c(0, 10), expand = c(0, 0)) +
  ggsci::scale_color_npg() +
  labs(x = "Wave", y = "Average number of contacts",
       subtitle = "First time participants only") +
  theme_bw() +
  theme(
    plot.subtitle = element_text(hjust = 0.5, size = 8),
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 8),
  )

plt_all <- ggplot(dt_all_boot, aes(x = ordered(wave), y = M)) +
  geom_point(aes(color = type),
             size = 2,
             position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(ymin = CL, ymax = CU, color = type),
                position = position_dodge(width = 0.4),
                width = 0.7) +
  scale_y_continuous(limits = c(0, 10), expand = c(0, 0)) +
  ggsci::scale_color_npg() +
  labs(x = "Wave", y = "Average number of contacts",
       subtitle = "All participants") +
  theme_bw() +
  theme(
    plot.subtitle = element_text(hjust = 0.5, size = 8),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 8)
  )

ggarrange(plt_w14, plt_all,
          ncol = 2,
          common.legend = TRUE,
          legend = "bottom")

ggsave(file.path("~/bayes-rate-consistency",
                 "paper/figures",
                 "sup-figure-tbd.jpeg"),
       units = "cm",
       width = 13.37,
       height = 9,
       dpi = 300)


library(data.table)
library(ggplot2)
library(pammtools)

dt <- readRDS("results/lrd-hsgp-5/intensity_matrix.rds")
dt[, comb := paste(gender, "to", alter_gender)]
dt.ff <- dt[gender == "Female" & alter_gender == "Female"]

ggplot(dt.ff[age %in% c(15, 45, 65)], aes(alter_age, intensity_M)) +
  geom_ribbon(aes(ymin = intensity_CL, ymax = intensity_CU, fill = paste("Wave", wave)),
                  alpha = 0.5) +
  geom_line(aes(color = paste("Wave", wave)), size = 0.4) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(limits = c(0,NA)) +
  scale_color_manual(values = c("#264653", "#2A9D8F", "#E9C46A", "#F4A261", "#E76F51")) +
  scale_fill_manual(values = c("#264653", "#2A9D8F", "#E9C46A", "#F4A261", "#E76F51")) +
  labs(x = "Contacts' age", y = "Contact intensity") +
  facet_grid(paste("Participants' age:", age) ~ paste("Wave", wave), scales = "free") +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 8),
    strip.text = element_text(size = 8, face = "bold"),
    strip.background = element_rect(color=NA, fill = "transparent"),
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 8)
  )

ggsave("paper/figures/sliced-intensities-ff.pdf",
       units = "cm", width = 14.7, height = 15, dpi=300)

#####################################################################
# FOR POSTER
#####################################################################
dt$age <- as.character(dt$age)
ggplot(dt[age %in% c(10, 40) & wave == 5], aes(x = alter_age, y = intensity_M)) +
  geom_step(size = 0.4, aes(x= alter_age, colour = age)) +
  pammtools:::geom_stepribbon(aes(ymin = intensity_CL, ymax = intensity_CU, fill = age ),colour = "transparent", alpha = 0.5) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(limits = c(0,NA)) +
  scale_color_manual(values = c( 'light salmon', 'plum')) +
  scale_fill_manual(values = c('light salmon', 'plum')) +
  labs(x = "Age of contacted individuals", y = "Contact intensity", fill = 'Age of contacting individuals') +
  facet_grid(paste0("Participants' age: ", age) ~ comb, scales = 'free') +
  theme_bw() +
  theme(
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 16),
    strip.text.x = element_text(size = 12, face = "bold"),
    strip.text.y = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 16),
    legend.position = 'none',
    strip.background = element_rect(fill = "transparent"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )

ggsave("poster/sliced-intensities-poster.png",
       units = "cm", width = 23, height = 12, dpi=300)


dt <- readRDS("results/lrd-hsgp-5/intensity_matrix.rds")

dt$age <- as.character(dt$age)
# dt.ff <- dt[gender == "Female" & alter_gender == "Female"]
# dt$gender <- ifelse(dt$gender == 'Female', 'Women', 'Men')
# dt$alter_gender <- ifelse(dt$alter_gender == 'Female', 'Women', 'Men')

dt[, comb := paste(gender, "to", alter_gender)]

ggplot(dt[age %in% c(10, 40) & wave == 5], aes(x = alter_age, y = intensity_M)) +
  geom_ribbon(aes(ymin = intensity_CL, ymax = intensity_CU, fill = paste("Age of contacting individuals:", age)), alpha = 0.5) +
  geom_line(size = 0.4, aes(color = paste("Age of contacting individuals:", age) )) +

  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(limits = c(0,NA)) +
  scale_color_manual(values = c( 'light salmon', 'plum')) +
  scale_fill_manual(values = c('light salmon', 'plum')) +
  labs(x = "Age of contacted individuals", y = "Contact intensity") +
  facet_grid(.~ comb, scales = 'free') +
  theme_bw() +
  theme(
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 16),
    strip.text.x = element_text(size = 12, face = "bold"),
    strip.text.y = element_text(size = 12, face = "bold"),
    # legend.title = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 16),
    legend.title = element_blank(),
    legend.position = 'bottom',
    strip.background = element_rect(fill = "transparent"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )

ggsave("poster/sliced-intensities-poster.png",
       units = "cm", width = 23, height = 10, dpi=300)

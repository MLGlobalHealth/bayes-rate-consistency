library(data.table)
library(ggplot2)

data <- readRDS("~/bayes-rate-consistency/data/COVIMOD/COVIMOD_multi_1527.rds")
dt_contacts <- data$contacts
dt_contacts[, t := y/S - y]

dt_sum <- dt_contacts[, .(y = sum(y),
                          t = sum(t)),
                      by = .(wave, age, gender)]
setnames(dt_sum, c("y", "t"), c("Individually reported", "Missing & aggregate"))

dt_sum <- melt(dt_sum,
               id.vars = c("wave", "age", "gender"),
               measure.vars = c("Individually reported", "Missing & aggregate"),
               variable.name = "type",
               value.name = "contacts")

dt_sum[, proportion := contacts / sum(contacts), by = .(wave, age, gender)]
dt_sum[, type := factor(type, levels = c("Missing & aggregate", "Individually reported"))]

ggplot(dt_sum[age < 85], aes(age, proportion)) +
  geom_bar(aes(fill = type), stat="identity") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), labels = scales::percent_format()) +
  scale_fill_manual(values = rev(c("#0D353F", "#F57D6C"))) +
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

ggsave("~/bayes-rate-consistency/paper/figures/sup-figure-2.jpeg",
       units = "cm", width = 18, height = 18, dpi = 300)

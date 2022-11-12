# The simulation part Pre or During COVID19 ----

# Preamble ----
# the simulation script aims to simulate the data set containing the total number of reports, nb of reports with detailed information by age-gender clusters.

cat("\n---------- Generating contact intensities ----------\n")

# Load the required packages
library(optparse)
library(data.table)
library(ggplot2)

# Define input arguments that can be changed by users
option_list <- list(
  optparse::make_option("--seed", type = "integer", default = 721L,
                        help = "Random number seed [default %default]",
                        dest = "seed"),
  optparse::make_option("--covid", type = "logical", default = FALSE,
                        help = "Simulate contact patterns in a hypothetical scenario with restricted contact patterns [default \"%default\"]",
                        dest = 'covid'),
  optparse::make_option("--repo_path", type = "character", default = "/rds/general/user/sd121/home/covimod-gp",
                        help = "Absolute file path to repository directory",
                        dest = 'repo.path')
)

args <- optparse::parse_args(optparse::OptionParser(option_list = option_list))

##### ---------- Error handling ---------- #####
if (!(args$covid %in% c(T,F))) {
  stop("--covid argument is invalid. Must be [TRUE, FALSE]")
}

if (is.na(args$repo.path)) {
  stop("Please specify --repo_path !")
}

##### ---------- Setup ---------- #####
# Setting up the export directory
export.path.part1 <- "data/simulations/intensity"
export.path.part2 <- ifelse(args$covid, "inCOVID", "preCOVID")
args$export.path <- file.path(args$repo.path, export.path.part1, export.path.part2)

if(!file.exists(args$export.path)){
  cat(paste("\n Making export directory:", args$export.path))
  dir.create(args$export.path)
}

# Directory for saving figures
fig.path <- file.path(args$export.path, "figures")
if(!dir.exists(fig.path)){
  cat(paste("\n Making figures directory:", fig.path))
  dir.create(fig.path)
}

# Source functions
source( file.path(args$repo.path, "R", "sim-intensity-utility.R") )

##### ---------- Helper functions ---------- #####
#' Simulates contact intensities/rates by age
#'
#' @param dsim
#' @param dpop Population data
#'
#' @return Simulated contact intesity/rate data.table
cntcts_sim_rates_by_age <- function(dsim, dpop)
{
  dsim[, DUMMY := 1L]
  tmp <- data.table( DUMMY = 1L, gender = c('Male','Female'))
  dsim <- merge(dsim, tmp, by = 'DUMMY', allow.cartesian = TRUE)
  tmpp <- dpop[, list(pop_t = sum(pop)), by = c('age')]
  tmpp <- merge(dpop, tmpp, by = 'age')
  tmpp[, pop_p := pop/pop_t]
  setnames(tmpp, c('age','gender'), c('alter_age','alter_gender') )
  dsim <- merge(dsim, tmpp, by = c('alter_age'), allow.cartesian = TRUE)
  set(dsim, NULL, 'cntct_intensity', dsim[, cntct_intensity*pop_p])
  dsim[, cntct_rate := cntct_intensity/pop]
  set(dsim, NULL, c('pop_t','pop', 'pop_p', 'DUMMY'), NULL)

  # Generate gender-gender contact rate patterns
  #
  # The contact rate cntct_rate has the symmetry property
  # Male-Female contact rate pattern and Female-Male pattern are symmetric
  # Male-Male and Female-Female patterns are self-symmetric
  # first select the whole pattern of Male-Female and convert to Female-Male pattern
  dMF <- dsim[gender == "Male" & alter_gender == "Female"]
  dFM <- copy(dMF)
  setnames(dFM, c("gender","age","alter_gender","alter_age"), c("alter_gender","alter_age","gender","age"))

  # next select the lower triangle pattern of Male-Male and Female-Female patterns, and fill out the other part by symmetry
  dMM <- dsim[gender == "Male" & alter_gender == "Male" & age >= alter_age]
  tmp <- copy(dMM)
  setnames(tmp, c("age","alter_age"), c("alter_age","age"))
  dMM <- rbind(tmp[age != alter_age], dMM)

  dFF <- dsim[gender == "Female" & alter_gender == "Female" & age >= alter_age]
  tmp <- copy(dFF)
  setnames(tmp, c("age","alter_age"), c("alter_age","age"))
  dFF <- rbind(tmp[age != alter_age], dFF)

  dsim <- rbind(dFM, dMF, dMM, dFF)
  set(tmpp, NULL, c('pop_t', 'pop_p'), NULL)
  dsim <- merge(dsim, tmpp, by = c('alter_age', 'alter_gender'))
  set(dsim, NULL, 'cntct_intensity', dsim[, cntct_rate * pop])

  return(dsim)
}

##### ---------- Define population sizes to determine contacts intensities and rates ---------- #####
# Set seed----
set.seed(args$seed)

cat("\n Define population sizes to determine contacts intensities and rates ... ")
dpop <- as.data.table(expand.grid(gender = c("Male", "Female"), age = 6:49))
tmp <- as.data.table(read.csv(file.path(args$repo.path, "data/germany-population-2011.csv")))
dpop <- merge(dpop, tmp, by = c("gender", "age"))

##### ---------- Generate contact intensities by age and gender ---------- #####
set.seed(args$seed)

if (!args$covid) {
  cat("\n Generate gender- and age-specific contact intensities before COVID19 ...")
  di <- cntcts_sim_intensities_precovid_diagonal()
}

if (args$covid) {
  cat("\n Generate gender- and age-specific contact intensities during COVID19 ...")
  di <- cntcts_sim_intensities_incovid_by_income_diagonal()
  di <- di[, .(cntct_intensity = mean(cntct_intensity)), by = c('age', 'alter_age')]
}

##### ---------- Generate contact rates ---------- #####
# Generate contact rates
cat("\n Generate gender- and age-specific contact rates ...")
di <- cntcts_sim_rates_by_age(di, dpop)

# Save the dataset
saveRDS(di, file = file.path(args$export.path, "data.rds"))

##### ---------- Visualization ---------- #####
setkey(di, alter_age, age, alter_gender, gender)

# Contact intensities
p <- ggplot(di) +
  geom_tile(aes(x = age, y = alter_age, fill = cntct_intensity)) +
  labs(x = "Participants' age", y = "Contacts' age", fill = "Contact intensity" ) +
  coord_equal() +
  facet_grid(alter_gender ~ gender) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  viridis::scale_fill_viridis(na.value = "white", option="H", limits=c(NA,1.3)) +
  guides(fill = guide_colorbar(title.position = "top", barwidth = 10)) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(color = NA, fill = "transparent")
  )

ggsave(file.path(fig.path, "intensities.pdf"), plot = p, width = 10, height = 6)

# Contact intensity aggregated by age
tmp <- di[, list(cntct_intensity = sum(cntct_intensity)), by = c('gender','age')]
p <- ggplot(tmp, aes(x = age, y = cntct_intensity) ) +
  geom_step(aes(colour = gender)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Participants' age", y = 'Contact intensity', color = 'Gender') +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(color = NA, fill = "transparent")
  )

ggsave(file.path(fig.path, "marginal-intensities.pdf"), plot = p, width = 6, height = 5)

cat("\n DONE. \n")

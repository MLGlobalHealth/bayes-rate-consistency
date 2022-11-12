# contact intensities assumption
# pre-covid
# child of age 7 should only have 30 contacts with children of age 6-18 this should sum to 30
cntcts_sim_intensities_precovid <- function()
{
  # target respondents in age 6-49
  di <- as.data.table(expand.grid(age = 6:49, alter_age = 6:49 ))
  di[, cntct_intensity := fcase(age %in% 6:18  & alter_age %in% 6:18, 30/13,
                                age %in% 6:18  & alter_age %in% 30:39, 2/10,
                                #age %in% 6:18  & alter_age %in% 40:49, 4/10,
                                age %in% 19:29 & alter_age %in% 19:29, 25/11,
                                age %in% 19:29 & alter_age %in% 40:49, 2/10,

                                age %in% 30:39 & alter_age %in% 30:39, 20/10,
                                #age %in% 30:39 & alter_age %in% 40:49, 4/10,
                                age %in% 30:39 & alter_age %in% 6:18,  4/13,
                                age %in% 40:49 & alter_age %in% 40:49, 15/10,
                                age %in% 40:49 & alter_age %in% 19:29, 4/11,
                                #age %in% 40:49 & alter_age %in% 6:18,  3,
                                default = 2/10)]

  return(di)
}

# in-covid
cntcts_sim_intensities_incovid_by_income <- function()
{
  # target respondents in age 6-49
  di <- as.data.table(expand.grid(age = 6:49, alter_age = 6:49 ))
  di[, income_level := 'low' ]
  di[, cntct_intensity := fcase(age %in% 6:18  & alter_age %in% 6:18, 1/13,
                                age %in% 6:18  & alter_age %in% 30:39, 2/10,
                                # age %in% 6:18  & alter_age %in% 40:49, 4,
                                age %in% 19:29 & alter_age %in% 19:29, 25/11,
                                age %in% 19:29 & alter_age %in% 40:49, 2/10,

                                age %in% 30:39 & alter_age %in% 30:39, 20/10,
                                age %in% 30:39 & alter_age %in% 40:49, 0.2,
                                age %in% 30:39 & alter_age %in% 6:18,  2/13,
                                age %in% 40:49 & alter_age %in% 40:49, 15/10,
                                age %in% 40:49 & alter_age %in% 30:39, 0.2,
                                age %in% 40:49 & alter_age %in% 19:29,  2/11,
                                default = 0)]
  tmp <- as.data.table(expand.grid(age = 6:49, alter_age = 6:49 ))
  tmp[, income_level := 'high' ]
  tmp[, cntct_intensity := fcase(age %in% 6:18  & alter_age %in% 6:18,  1/13,
                                 age %in% 6:18  & alter_age %in% 30:39, 2/10,
                                 age %in% 19:29 & alter_age %in% 19:29, 5/11,
                                 age %in% 19:29 & alter_age %in% 40:49, 2/10,

                                 age %in% 30:39 & alter_age %in% 30:39, 5/10,
                                 age %in% 30:39 & alter_age %in% 6:18,  2/13,
                                 age %in% 40:49 & alter_age %in% 40:49, 5/10,
                                 age %in% 40:49 & alter_age %in% 19:29, 2/11,

                                 default = 0)]
  di <- rbind(di, tmp)

  return(di)
}

cntcts_sim_intensities_precovid_default_zero <- function()
{
  # target respondents in age 6-49
  di <- as.data.table(expand.grid(age = 6:49, alter_age = 6:49 ))
  di[, cntct_intensity := fcase(age %in% 6:18  & alter_age %in% 6:18, 30/13,
                                age %in% 6:18  & alter_age %in% 30:39, 2/10,
                                #age %in% 6:18  & alter_age %in% 40:49, 4/10,
                                age %in% 19:29 & alter_age %in% 19:29, 25/11,
                                age %in% 19:29 & alter_age %in% 40:49, 2/10,

                                age %in% 30:39 & alter_age %in% 30:39, 20/10,
                                #age %in% 30:39 & alter_age %in% 40:49, 4/10,
                                age %in% 30:39 & alter_age %in% 6:18,  4/13,
                                age %in% 40:49 & alter_age %in% 40:49, 15/10,
                                age %in% 40:49 & alter_age %in% 19:29, 4/11,
                                #age %in% 40:49 & alter_age %in% 6:18,  3,
                                default = 0)]

  return(di)
}
# in-covid
cntcts_sim_intensities_incovid_by_income_default_zero <- function()
{
  # target respondents in age 6-49
  di <- as.data.table(expand.grid(age = 6:49, alter_age = 6:49 ))
  di[, income_level := 'low' ]
  di[, cntct_intensity := fcase(age %in% 6:18  & alter_age %in% 6:18, 1/13,
                                age %in% 6:18  & alter_age %in% 30:39, 2/10,
                                # age %in% 6:18  & alter_age %in% 40:49, 4,
                                age %in% 19:29 & alter_age %in% 19:29, 25/11,
                                age %in% 19:29 & alter_age %in% 40:49, 2/10,

                                age %in% 30:39 & alter_age %in% 30:39, 20/10,
                                age %in% 30:39 & alter_age %in% 6:18,  2/13,
                                age %in% 40:49 & alter_age %in% 40:49, 15/10,
                                age %in% 40:49 & alter_age %in% 19:29,  2/11,
                                default = 0)]
  tmp <- as.data.table(expand.grid(age = 6:49, alter_age = 6:49 ))
  tmp[, income_level := 'high' ]
  tmp[, cntct_intensity := fcase(age %in% 6:18  & alter_age %in% 6:18,  1/13,
                                 age %in% 6:18  & alter_age %in% 30:39, 2/10,
                                 age %in% 19:29 & alter_age %in% 19:29, 5/11,
                                 age %in% 19:29 & alter_age %in% 40:49, 2/10,

                                 age %in% 30:39 & alter_age %in% 30:39, 5/10,
                                 age %in% 30:39 & alter_age %in% 6:18,  2/13,
                                 age %in% 40:49 & alter_age %in% 40:49, 5/10,
                                 age %in% 40:49 & alter_age %in% 19:29, 2/11,

                                 default = 0)]
  di <- rbind(di, tmp)

  return(di)
}

cntcts_sim_intensities_precovid_diagonal <- function()
{
  # target respondents in age 6-49
  di <- as.data.table(expand.grid(age = 6:49, alter_age = 6:49 ))
  di[, diff_age := age - alter_age]
  di[, cntct_intensity := fcase(
                                age %in% 6:18  & diff_age %in% -8:8, pmax(0, 2.5 - 0.2 * abs(diff_age)),
                                age %in% 6:18  & diff_age %in% c(-11:-9,9:11), 0.1,
                                age %in% 6:18  & diff_age %in% c(-13:-12,12:13), 0.03,
                                age %in% 6:18  & diff_age %in% c(-15:-14,14:15), 0.01,
                                age %in% 6:18  & diff_age %in% -25:-23, pmax(0, 0.8 - 0.5 * abs(diff_age + 24)),
                                age %in% 6:18  & diff_age %in% c(-27:-26, -22:-21), 0.1,
                                age %in% 6:18  & diff_age %in% c(-29:-28, -20:-19), 0.01,


                                age %in% 19:29  & diff_age %in% -5:5, pmax(0, 2.5 - 0.3 * abs(diff_age)),
                                age %in% 19:29  & diff_age %in% c(-9:-6,6:9), 0.8,
                                age %in% 19:29  & diff_age %in% c(-13:-10,10:13), 0.04,
                                age %in% 19:29  & diff_age %in% c(-15:-14,14:15), 0.01,
                                age %in% 19:29  & diff_age %in% -25:-23, pmax(0, 0.8 - 0.5 * abs(diff_age + 24)),
                                age %in% 19:29  & diff_age %in% c(-27:-26, -22:-21), 0.1,
                                age %in% 19:29  & diff_age %in% c(-29:-28, -20:-19), 0.01,

                                age %in% 25:29  & diff_age %in% 21:22, 0.2,
                                age %in% 25:29  & diff_age %in% 19:20, 0.02,
                                age %in% 29  & diff_age %in% 23, 0.6,
                                age %in% 30:39  & diff_age %in% -5:5, pmax(0, 2 - 0.24 * abs(diff_age)),
                                age %in% 30:39  & diff_age %in% c(-9:-6,6:9), 0.64,
                                age %in% 30:39  & diff_age %in% c(-13:-10,10:13), 0.03,
                                age %in% 30:39  & diff_age %in% c(-15:-14,14:15), 0.01,
                                age %in% 30:39  & diff_age %in% 23:25, pmax(0, 1.6 - 1 * abs(diff_age - 24)),
                                age %in% 30:39  & diff_age %in% c(21:22, 26:27), 0.2,
                                age %in% 30:39  & diff_age %in% c(19:20, 28:29), 0.02,

                                age %in% 40:49  & diff_age %in% -5:5, pmax(0, 1.5 - 0.18 * abs(diff_age)),
                                age %in% 40:49  & diff_age %in% c(-9:-6,6:9), 0.5,
                                age %in% 40:49  & diff_age %in% c(-13:-10,10:13), 0.02,
                                age %in% 40:49  & diff_age %in% c(-15:-14,14:15), 0.007,
                                age %in% 40:49  & diff_age %in% 23:25, pmax(0, 1.6 - 1 * abs(diff_age - 24)),
                                age %in% 40:49  & diff_age %in% c(21:22, 26:27), 0.2,
                                age %in% 40:49  & diff_age %in% c(19:20, 28:29), 0.02,

                                default = 0)]
  set(di, NULL, 'diff_age', NULL)
  return(di)
}
# in-covid
cntcts_sim_intensities_incovid_by_income_diagonal <- function()
{
  # target respondents in age 6-49
  di <- as.data.table(expand.grid(age = 6:49, alter_age = 6:49 ))
  di[, income_level := 'low' ]
  di[, diff_age := age - alter_age]
  di[, cntct_intensity := fcase(age %in% 6:10  & diff_age %in% -8:8, pmax(0, 0.08 - 0.007 * abs(diff_age)),
                                age %in% 6:10  & diff_age %in% c(-11:-9,9:11), 0.003,
                                age %in% 6:10  & diff_age %in% c(-13:-12,12:13), 1e-3,
                                age %in% 6:10  & diff_age %in% c(-15:-14,14:15), 3*1e-4,
                                age %in% 6:10  & diff_age %in% -25:-23, pmax(0, 0.8 - 0.5 * abs(diff_age + 24)),
                                age %in% 6:10  & diff_age %in% c(-27:-26, -22:-21), 0.1,
                                age %in% 6:10  & diff_age %in% c(-29:-28, -20:-19), 0.01,

                                age %in% 11:18 & diff_age %in% -8:8, pmax(0, 0.4 - 0.035 * abs(diff_age)),
                                age %in% 11:18 & diff_age %in% c(-11:-9,9:11), 0.015,
                                age %in% 11:18 & diff_age %in% c(-13:-12,12:13), 5e-3,
                                age %in% 11:18 & diff_age %in% c(-15:-14,14:15), 15*1e-4,
                                age %in% 11:18  & diff_age %in% -25:-23, pmax(0, 0.8 - 0.5 * abs(diff_age + 24)),
                                age %in% 11:18  & diff_age %in% c(-27:-26, -22:-21), 0.1,
                                age %in% 11:18  & diff_age %in% c(-29:-28, -20:-19), 0.01,


                                age %in% 19:29  & diff_age %in% -5:5, pmax(0, 2.5 - 0.3 * abs(diff_age)),
                                age %in% 19:29  & diff_age %in% c(-9:-6,6:9), 0.8,
                                age %in% 19:29  & diff_age %in% c(-13:-10,10:13), 0.04,
                                age %in% 19:29  & diff_age %in% c(-15:-14,14:15), 0.01,
                                age %in% 19:29  & diff_age %in% -25:-23, pmax(0, 0.8 - 0.5 * abs(diff_age + 24)),
                                age %in% 19:29  & diff_age %in% c(-27:-26, -22:-21), 0.1,
                                age %in% 19:29  & diff_age %in% c(-29:-28, -20:-19), 0.01,

                                age %in% 25:29  & diff_age %in% 21:22, 0.1,
                                age %in% 25:29  & diff_age %in% 19:20, 0.01,
                                age %in% 29  & diff_age %in% 23, 0.6,
                                age %in% 30:39  & diff_age %in% -5:5, pmax(0, 2 - 0.24 * abs(diff_age)),
                                age %in% 30:39  & diff_age %in% c(-9:-6,6:9), 0.64,
                                age %in% 30:39  & diff_age %in% c(-13:-10,10:13), 0.03,
                                age %in% 30:39  & diff_age %in% c(-15:-14,14:15), 0.01,
                                age %in% 30:39  & diff_age %in% 23:25, pmax(0, 0.8 - 0.5 * abs(diff_age - 24)),
                                age %in% 30:39  & diff_age %in% c(21:22, 26:27), 0.1,
                                age %in% 30:39  & diff_age %in% c(19:20, 28:29), 0.01,

                                age %in% 40:49  & diff_age %in% -5:5, pmax(0, 1.5 - 0.18 * abs(diff_age)),
                                age %in% 40:49  & diff_age %in% c(-9:-6,6:9), 0.5,
                                age %in% 40:49  & diff_age %in% c(-13:-10,10:13), 0.02,
                                age %in% 40:49  & diff_age %in% c(-15:-14,14:15), 0.007,
                                age %in% 40:49  & diff_age %in% 23:25, pmax(0, 0.8 - 0.5 * abs(diff_age - 24)),
                                age %in% 40:49  & diff_age %in% c(21:22, 26:27), 0.1,
                                age %in% 40:49  & diff_age %in% c(19:20, 28:29), 0.01,

                                default = 0)]

  tmp <- as.data.table(expand.grid(age = 6:49, alter_age = 6:49 ))
  tmp[, income_level := 'high' ]
  tmp[, diff_age := age - alter_age]
  tmp[, cntct_intensity := fcase(age %in% 6:10  & diff_age %in% -8:8, pmax(0, 0.08 - 0.007 * abs(diff_age)),
                                 age %in% 6:10  & diff_age %in% c(-11:-9,9:11), 0.003,
                                 age %in% 6:10  & diff_age %in% c(-13:-12,12:13), 1e-3,
                                 age %in% 6:10  & diff_age %in% c(-15:-14,14:15), 3*1e-4,
                                 age %in% 6:10  & diff_age %in% -25:-23, pmax(0, 0.8 - 0.5 * abs(diff_age + 24)),
                                 age %in% 6:10  & diff_age %in% c(-27:-26, -22:-21), 0.1,
                                 age %in% 6:10  & diff_age %in% c(-29:-28, -20:-19), 0.01,

                                 age %in% 11:18 & diff_age %in% -8:8, pmax(0, 0.4 - 0.035 * abs(diff_age)),
                                 age %in% 11:18 & diff_age %in% c(-11:-9,9:11), 0.015,
                                 age %in% 11:18 & diff_age %in% c(-13:-12,12:13), 5e-3,
                                 age %in% 11:18 & diff_age %in% c(-15:-14,14:15), 15*1e-4,
                                 age %in% 11:18  & diff_age %in% -25:-23, pmax(0, 0.8 - 0.5 * abs(diff_age + 24)),
                                 age %in% 11:18  & diff_age %in% c(-27:-26, -22:-21), 0.1,
                                 age %in% 11:18  & diff_age %in% c(-29:-28, -20:-19), 0.01,


                                age %in% 19:29  & diff_age %in% -5:5, pmax(0, 0.5 - 0.06 * abs(diff_age)),
                                age %in% 19:29  & diff_age %in% c(-9:-6,6:9), 0.16,
                                age %in% 19:29  & diff_age %in% c(-13:-10,10:13), 0.008,
                                age %in% 19:29  & diff_age %in% c(-15:-14,14:15), 0.002,
                                age %in% 19:29  & diff_age %in% -25:-23, pmax(0, 0.8 - 0.5 * abs(diff_age + 24)),
                                age %in% 19:29  & diff_age %in% c(-27:-26, -22:-21), 0.1,
                                age %in% 19:29  & diff_age %in% c(-29:-28, -20:-19), 0.01,

                                age %in% 30:39  & diff_age %in% -5:5, pmax(0, 0.5 - 0.06 * abs(diff_age)),
                                age %in% 30:39  & diff_age %in% c(-9:-6,6:9), 0.16,
                                age %in% 30:39  & diff_age %in% c(-13:-10,10:13), 0.0075,
                                age %in% 30:39  & diff_age %in% c(-15:-14,14:15), 0.0025,
                                age %in% 30:39  & diff_age %in% 23:25, pmax(0, 0.8 - 0.5 * abs(diff_age - 24)),
                                age %in% 30:39  & diff_age %in% c(21:22, 26:27), 0.1,
                                age %in% 30:39  & diff_age %in% c(19:20, 28:29), 0.01,
                                age %in% 25:29  & diff_age %in% 21:22, 0.1,
                                age %in% 25:29  & diff_age %in% 19:20, 0.01,
                                age %in% 29  & diff_age %in% 23, 0.6,

                                age %in% 40:49  & diff_age %in% -5:5, pmax(0, 0.5 - 0.06 * abs(diff_age)),
                                age %in% 40:49  & diff_age %in% c(-9:-6,6:9), 0.16,
                                age %in% 40:49  & diff_age %in% c(-13:-10,10:13), 0.0075,
                                age %in% 40:49  & diff_age %in% c(-15:-14,14:15), 0.0025,
                                age %in% 40:49  & diff_age %in% 23:25, pmax(0, 0.8 - 0.5 * abs(diff_age - 24)),
                                age %in% 40:49  & diff_age %in% c(21:22, 26:27), 0.1,
                                age %in% 40:49  & diff_age %in% c(19:20, 28:29), 0.01,
                                default = 0)]
  di <- rbind(di, tmp)
  set(di, NULL, 'diff_age', NULL)
  return(di)
}
# make age-income population
simcntcts_make_population_counts_by_age_income <- function(pop_size, pop_proportion_high_income, prj.dir, infile.pop.m, infile.pop.f)
{
  # age-specific prob
  # get the age distribution
  dpop <- as.data.table(expand.grid(gender = c("Male", "Female"), age = 6:49))
  tmp <- as.data.table(read.csv(file.path(prj.dir, infile.pop.m)))
  tmp[, gender := "Male"]
  tmp2 <- as.data.table(read.csv(file.path(prj.dir, infile.pop.f)))
  tmp2[, gender := "Female"]
  tmp <- rbind(tmp, tmp2)
  dpop <- merge(dpop, tmp, by = c("gender", "age"))
  dpop[, pop_prop := pop/sum(pop)]

  # assume equal number of individuals in each age / gender cat in the whole pop
  tmp <- subset(dpop, select = c(age, gender))
  tmp[, pop_ag := pop_size / nrow(tmp)]
  dpop <- merge(dpop, tmp, by = c("age","gender"))
  dpop[, DUMMY := 1L]
  tmp <- data.table( DUMMY = 1L, income_level = c("high","low"))
  dpop <- merge(dpop, tmp, by = "DUMMY", allow.cartesian = TRUE)

  # make population sizes in each pop strata
  dpop[, pop_prop := pop_prop * fcase(
    income_level == "high", pop_proportion_high_income,
    income_level != "high", 1 - pop_proportion_high_income
  )]
  dpop[, pop := pop_ag  * pop_prop]
  dpop[, pop := pop * pop_size / sum(pop)]
  set(dpop, NULL, c("DUMMY", "pop_prop", "pop_ag"), NULL)
  return(dpop)
}

simcntcts_make_population_counts_by_income <- function(pop_size, pop_proportion_high_income)
{
  dpop <- as.data.table(expand.grid(gender = c("Male", "Female"), age = 6:49, income_level = c('high', 'low')))
  tmp <- unique(subset(dpop, select = c(age, gender)))
   # assume equal number of individuals in each age / gender cat
  tmp[, pop_ag := pop_size / nrow(tmp)]
  dpop <- merge(dpop, tmp, by = c("age","gender"))

  # make population sizes in each pop strata
  dpop[, pop := pop_ag  * fcase(
    income_level == "high", pop_proportion_high_income,
    income_level != "high", 1 - pop_proportion_high_income
  )]
  return(dpop)
}

simcntcts_make_population_counts_by_age <- function(pop_size, prj.dir, infile.pop.m, infile.pop.f)
{
  # age-specific prob
  # get the age distribution
  dpop <- as.data.table(expand.grid(gender = c("Male", "Female"), age = 6:49))
  tmp <- as.data.table(read.csv(file.path(prj.dir, infile.pop.m)))
  tmp[, gender := "Male"]
  tmp2 <- as.data.table(read.csv(file.path(prj.dir, infile.pop.f)))
  tmp2[, gender := "Female"]
  tmp <- rbind(tmp, tmp2)
  dpop <- merge(dpop, tmp, by = c("gender", "age"))
  dpop[, pop_prop := pop/sum(pop)]

  # assume equal number of individuals in each age / gender cat in the whole pop
  tmp <- subset(dpop, select = c(age, gender))
  tmp[, pop_ag := pop_size / nrow(tmp)]
  dpop <- merge(dpop, tmp, by = c("age","gender"))
  dpop[, pop := pop_ag  * pop_prop]
  dpop[, pop := pop * pop_size / sum(pop)]
  set(dpop, NULL, c("pop_prop", "pop_ag"), NULL)
  return(dpop)
}

simcntcts_make_population_counts <- function(pop_size)
{
  # TODO: do we need to redistribute the pop?
  dpop <- as.data.table(expand.grid(gender = c("Male", "Female"), age = 6:49, income_level = c('high', 'low')))
  tmp <- unique(subset(dpop, select = c(age, gender)))
   # assume equal number of individuals in each age / gender cat
  tmp[, pop_ag := pop_size / nrow(tmp)]
  dpop <- merge(dpop, tmp, by = c("age","gender"))
  return(dpop)
}


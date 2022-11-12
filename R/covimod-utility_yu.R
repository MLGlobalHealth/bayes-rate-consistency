# Helper functions for working with COVIMOD data

# TODO: Consider group contacts

load_covimod_data <- function(){
  covimod.path <- file.path("data/COVIMOD/COVIMOD_data_2022-03-21.RData")
  load(covimod.path)

  pop.path <- file.path("data/germany-population-2011.csv")
  dt.pop <- as.data.table(read.csv(pop.path))

  # Convert to data.table
  dt.hh <- as.data.table(hh_data)
  dt.nhh <- as.data.table(non_hh_data)
  dt.part <- as.data.table(part_data)

  # Convert from character to binary
  dt.hh[, hh_met_this_day := ifelse(hh_met_this_day == "Yes", 1, 0)]

  # Preprocess age and gender labels
  dt.part <- process_age_gender_labels(dt.part)
  dt.hh <- process_age_gender_labels(dt.hh)
  dt.nhh <- process_age_gender_labels(dt.nhh)

  # Change column names
  setnames(dt.part, c("wave_0", "age_0", "sex", "age_group"), c("wave", "age", "gender", "age_strata"))
  setnames(dt.hh, old = c("age_group", "sex"), new = c("alter_age_strata", "alter_gender"))
  setnames(dt.nhh, old = c("age_group", "sex"), new = c("alter_age_strata", "alter_gender"))

  return( list(part = dt.part, pop = dt.pop, hh = dt.hh, nhh = dt.nhh) )
}

#' Creates a summary table for each contact wave
#'
#' @param .wave Wave number
#' @param dt.hh data.table with household contact info
#' @param dt.nhh data.table with non-household contact info
#'
#' @return
#' @export
#'
#' @examples
make_contact_summary <- function(.wave, dt.part, dt.hh, dt.nhh){
  hh.sum <- dt.hh[, .(hh_cntct = sum(hh_met_this_day)), by=list(new_id, wave)]
  nhh.sum <- dt.nhh[, .(nhh_cntct = .N), by=list(new_id, wave)]

  # Remove or stratify age groups
  tmp <- merge(dt.part[, list(new_id, wave, age_strata)], hh.sum, all.x=T)
  tmp <- merge(tmp, nhh.sum, all.x = T)

  tmp[, hh_cntct := ifelse(is.na(hh_cntct), 0, hh_cntct)]
  tmp[, nhh_cntct := ifelse(is.na(nhh_cntct), 0, nhh_cntct)]

  tmp[, cntct := hh_cntct + nhh_cntct]
  dt.sum <- tmp[, .(N = .N,
                       cntct_mean = mean(cntct),
                       cntct_sd = sd(cntct),
                       cntct_min = min(cntct),
                       cntct_max = max(cntct)),
                   by = list(wave, age_strata)]

  dt.sum <- dt.sum[wave == .wave]
  dt.sum[order(age_strata)]
}

#' Creates the contact table for a given wave of the COVIMOD data
#'
#' @param .wave wave number
#' @param dt.part data.table with participant info
#' @param dt.hh data.table with household contact info
#' @param dt.nhh data.table with non-household contact info
#' @param dt.pop data.table with population counts
#'
#' @return A data.table with contact counts for the given wave
#' @export
#'
#' @examples
make_contact_table <- function(.wave, dt.part, dt.hh, dt.nhh, dt.pop){
  part <- dt.part[wave == .wave]
  hh <- dt.hh[wave == .wave]
  nhh <- dt.nhh[wave == .wave]

  # Total number of survey participants by age and gender
  part.sum <- part[, .(part = .N), by=list(imp_age, gender)]

  # Household contacts
  hh.cnt <- merge(
    part[,list(new_id, imp_age, age_strata, gender)],
    hh[hh_met_this_day==1, list(new_id, alter_age_strata, alter_gender)],
    all.y=T
  )
  hh.cnt <- hh.cnt[, .(y_hh = .N),
                   by=list(imp_age, gender, alter_age_strata, alter_gender)]

  # Non-household contacts
  nhh.cnt <- merge(
    part[,list(new_id, imp_age, age_strata, gender)],
    nhh[,list(new_id, alter_age_strata, alter_gender)],
    all.y = T
  )
  nhh.cnt <- nhh.cnt[, .(y_nhh = .N),
                     by=list(imp_age, gender, alter_age_strata, alter_gender)]

  dt.cnt <- merge(hh.cnt, nhh.cnt, all.x=T, all.y=T)
  dt.cnt[, y_hh := ifelse(is.na(y_hh), 0, y_hh)]
  dt.cnt[, y_nhh := ifelse(is.na(y_nhh), 0, y_nhh)]
  dt.cnt[, y := y_hh + y_nhh]
  dt.cnt <- drop_na(dt.cnt)

  dt.cnt <- merge(dt.cnt, part.sum, all.x = TRUE, by=c("imp_age", "gender"))

  tmp <- copy(dt.pop)
  tmp <- setnames(tmp, c("gender", "age_strata"), c("alter_gender", "alter_age_strata"))
  tmp <- tmp[, .(pop = sum(pop)), by=list(alter_gender, alter_age_strata)]

  dt.cnt <- merge(dt.cnt, tmp, all.x=TRUE, by=c("alter_gender", "alter_age_strata"))
  dt.cnt[, wave := .wave]

  return(dt.cnt)
}

age_stratify <- function(dt, name="age", alter_name="age_strata"){
  dt <- copy(dt)

  setnames(dt, name, "tmp_age")
  dt[, tmp_age_strata := fcase(
    tmp_age <= 4,  "0-4",
    tmp_age <= 9,  "5-9",
    tmp_age <= 14, "10-14",
    tmp_age <= 19, "15-19",
    tmp_age <= 24, "20-24",
    tmp_age <= 34, "25-34",
    tmp_age <= 44, "35-44",
    tmp_age <= 54, "45-54",
    tmp_age <= 64, "55-64",
    tmp_age <= 69, "65-69",
    tmp_age <= 74, "70-74",
    tmp_age <= 79, "75-79",
    tmp_age <= 84, "80-84",
    tmp_age > 84, "85+",
    default = NA
  )]

  dt$tmp_age_strata <- factor(
    dt$tmp_age_strata,
    levels = c("0-4","5-9","10-14","15-19","20-24","25-34","35-44",
               "45-54","55-64","65-69","70-74","75-79","80-84","85+")
  )

  setnames(dt, "tmp_age", name)
  setnames(dt, "tmp_age_strata", alter_name)

  return(dt)
}

process_age_gender_labels <- function(dt){
  dt$age_group <- dplyr::case_when(
    dt$age_group == "<1" ~ "0-4",
    dt$age_group == "Under 1" ~ "0-4",
    dt$age_group == "1-4" ~ "0-4",
    dt$age_group == "85 years or older" ~ "85+",
    TRUE ~ dt$age_group
  )

  dt$age_group <- factor(
    dt$age_group,
    levels = c("0-4","5-9","10-14","15-19","20-24","25-34","35-44",
               "45-54","55-64","65-69","70-74","75-79","80-84","85+")
  )

  dt$sex <- factor(dt$sex, levels = c("Male", "Female"))
  return(dt)
}

#' Pool COVIMOD surveys
#'
#' @param individual a data.table with aggregated individual contact counts
#' @param group a data.table with group contact counts
#' @param waves a numeric vector of wave numbers to pool together [default: all waves]
#' @param max.age maximum age of participant to include [default: 84]
#'
#' @return
#' @export
#'
#' @examples
make_covimod_dataset <- function(individual, group=NULL, waves = seq(1,33), type="all", max.age = 84){
  age_strata <- c("0-4","5-9","10-14","15-19","20-24","25-34","35-44",
                  "45-54","55-64","65-69","70-74","75-79","80-84")

  # Create grid
  dt <- as.data.table(expand.grid(age = seq(0,84), gender = c("Male", "Female"),
                                  alter_age_strata = age_strata, alter_gender = c("Male", "Female")))

  # Extract all age and gender info present in participants
  individual <- individual[wave %in% waves]
  dt.part <- unique(individual[, list(new_id, imp_age, gender)])
  dt.part <- dt.part[, list(part = .N), by=c("imp_age", "gender")]
  setnames(dt.part, "imp_age", "age")

  # Merge participant info
  dt <- merge(dt, dt.part, by=c("age", "gender"), all.x=TRUE)
  dt[is.na(part), part := 0]

  if (type == "hh"){
    # Household contacts only
    individual <- individual[type == "hh"]
  } else if (type == "nhh"){
    # Non-household contacts only
    individual <- individual[type == "nhh"]
  }

  # Calculate contact counts
  dt.cnt <-individual[imp_age <= max.age]
  dt.cnt <- dt.cnt[alter_age_strata != "85+"]

  dt.cnt <- dt.cnt[, .(y = sum(y)), by=c("imp_age", "gender", "alter_age_strata", "alter_gender")]
  setnames(dt.cnt, "imp_age", "age")

  # Merge counts
  dt <- merge(dt, dt.cnt, by=c("age","gender","alter_age_strata", "alter_gender"), all.x = TRUE)

  if(!is.null(group) & type != "hh"){
    dt.indiv <- dt.cnt[, list(y_indiv = sum(y)), by=list(age, gender)]

    # Group contacts
    dt.group <- group[wave %in% waves, list(y_group = sum(y_group)), by=list(imp_age, gender)]
    setnames(dt.group, "imp_age", "age")

    dt.group <- merge(dt.indiv, dt.group, by=c("age", "gender"), all.x = TRUE)

    # Calculate sampling fraction term rho
    dt.group[is.na(y_group), y_group := 0]
    dt.group[, y_total := y_indiv + y_group]
    dt.group[, rho := y_total/y_indiv]
    set(dt.group, which(is.nan(dt.group$rho)), "rho", 1)
    set(dt.group, which(is.infinite(dt.group$rho)), "rho", 1)

    # merge individual and group data
    dt <- merge(dt, dt.group[, list(age, gender, rho)], by=c("age", "gender"), all.x = TRUE)
  } else {
    dt[, rho := 1]
  }

  dt[is.na(y) & part > 0, y := 0] # Fill in non-missing
  dt[is.na(rho) & part > 0, rho := 0]
  dt <- dt[part != 0] # Remove true missing

  return(dt)
}

runif.int <- function(min, max) floor(runif(1, min = min, max = max + 0.999))

impute_child_age <- function(dt.part, seed=1527){
  set.seed(seed)

  tmp <- unique(dt.part[, list(new_id, wave, age, age_strata)])
  tmp[, min_age := as.numeric(stringr::str_extract(age_strata, "[0-9]{1,2}"))]
  tmp[, max_age := as.numeric(stringr::str_extract(age_strata, "[0-9]{1,2}$"))]

  tmp[is.na(age), imp_age := runif.int(min_age, max_age), by=.(new_id)]
  tmp[!is.na(age), imp_age := age]

  dt.part <- merge(dt.part, tmp[, list(new_id, wave, imp_age)])

  return(dt.part)
}



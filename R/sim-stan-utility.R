#' Initialize Stan data object
#'
#' @param strata Age stratification scheme
#' @return list
#'
#' @examples stan_data <- init_stan_data()
init_stan_data <- function(strata = "COVIMOD"){
  strata_config <- list(
    "COVIMOD" = 7,
    "CoMix" = 5,
    "5yr" = 9,
    "3yr" = 15
  )

  list(A=44, C = strata_config[[strata]])
}

 # Add N
add_N <- function(stan_data, dt){
  d <- unique(dt[part > 0, list(age, gender, alter_age_strata, alter_gender, y_strata)])

  stan_data$N_MM <- nrow(d[gender == "Male" & alter_gender == "Male"])
  stan_data$N_FF <- nrow(d[gender == "Female" & alter_gender == "Female"])
  stan_data$N_MF <- nrow(d[gender == "Male" & alter_gender == "Female"])
  stan_data$N_FM <- nrow(d[gender == "Female" & alter_gender == "Male"])

  return(stan_data)
}

# Add contacts Y_MM, Y_FF, Y_MF, Y_FM
add_contacts <- function(stan_data, dt){
  d <- unique(dt[, list(age, gender, alter_age_strata, alter_gender, y_strata)])
  d <- d[order(age, alter_age_strata)]

  stan_data$Y_MM <- d[gender == "Male" & alter_gender == "Male"]$y_strata
  stan_data$Y_FF <- d[gender == "Female" & alter_gender == "Female"]$y_strata
  stan_data$Y_MF <- d[gender == "Male" & alter_gender == "Female"]$y_strata
  stan_data$Y_FM <- d[gender == "Female" & alter_gender ==  "Male"]$y_strata

  return(stan_data)
}

# Add row major idx
make_row_major_idx <- function(dt, .gender, .alter_gender, C){
  d <- dt[gender == .gender & alter_gender == .alter_gender]

  d[, age_idx := age-5]
  d[, alter_age_strata_idx := as.numeric(alter_age_strata)]
  d[, row_major_idx := (age_idx-1)*C + alter_age_strata_idx]

  d <- d[part > 0]
  d <- d[order(age_idx, alter_age_strata_idx)]

  return(d$row_major_idx)
}

add_row_major_idx <- function(stan_data, dt){
  d <- unique(dt[, list(age, gender, alter_age_strata, alter_gender, part, y_strata)])
  C <- stan_data$C

  stan_data$ROW_MAJOR_IDX_MM <- make_row_major_idx(d, "Male", "Male", C)
  stan_data$ROW_MAJOR_IDX_FF <- make_row_major_idx(d, "Female", "Female", C)
  stan_data$ROW_MAJOR_IDX_MF <- make_row_major_idx(d, "Male", "Female", C)
  stan_data$ROW_MAJOR_IDX_FM <- make_row_major_idx(d, "Female", "Male", C)

  return(stan_data)
}

add_partsize_offsets <- function(stan_data, dt){
  d <- unique(dt[, list(age, gender, part)])
  d[part == 0, part := 1]

  stan_data$log_N_M <- log(d[gender == "Male"]$part)
  stan_data$log_N_F <- log(d[gender == "Female"]$part)

  return(stan_data)
}

# Add group offsets (For simulated data we won't consider group offsets)
add_group_offsets <- function(stan_data){
  A <- stan_data$A

  stan_data$log_S_M <- rep(0,A)
  stan_data$log_S_F <- rep(0,A)

  return(stan_data)
}

# Add population offsets
add_pop_offsets <- function(stan_data, dt){
  d <- unique(dt[, list(alter_age, alter_gender, pop)])
  d <- d[order(alter_age)]

  stan_data$log_P_M <- log(d[alter_gender == "Male"]$pop)
  stan_data$log_P_F <- log(d[alter_gender == "Female"]$pop)

  return(stan_data)
}

# Map age to age strata
add_map_age_to_strata <- function(stan_data, strata = "COVIMOD"){
  age_strata_list <- list(
    "COVIMOD" = c("6-9","10-14","15-19","20-24","25-34","35-44","45-49"),
    "CoMix" = c("6-11", "12-17", "18-29", "30-39", "40-49"),
    "5yr" = c("6-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"),
    "3yr" = c("6-8", "9-11","12-14","15-17","18-20","21-23","24-26","27-29",
              "30-32","33-35","36-38","39-41", "42-44","45-47","48-49")
  )

  age_strata <- age_strata_list[[strata]]
  alter_age_min <- as.numeric(str_extract(as.character(age_strata), "^[0-9]{1,2}"))

  strata_min <- unique(alter_age_min)
  strata_min_idx <- strata_min - min(strata_min) + 1

  A <- stan_data$A
  C <- stan_data$C

  map_age_to_strata <- matrix(0, nrow=A, ncol=C)
  for (c in 1:C) {
    if (c == C) {
      map_age_to_strata[strata_min_idx[c]:A, c] <- 1
    } else {
      map_age_to_strata[strata_min_idx[c]:(strata_min_idx[c+1]-1), c] <- 1
    }
  }

  stan_data$map_age_to_strata <- map_age_to_strata

  return(stan_data)
}

# Add non-nuisance index
add_nn_idx <- function(stan_data){
  A <- stan_data$A

  NN_IDX <- rep(NA, A*A)
  for (i in 1:A){
    min_idx <- (2*A-1)*(i-1) + (A-i+1)
    max_idx <- (2*A-1)*(i-1) + (2*A-i)
    NN_IDX[A*(i-1)+1:A*1] <- seq.int(min_idx, max_idx, 1)
  }

  stan_data$NN_IDX <- NN_IDX
  return(stan_data)
}

# Add standardized age index
add_std_age_idx <- function(stan_data){
  A <- stan_data$A

  age_idx <- seq.int(0,A-1,1)
  diff_idx <- seq.int(-A+1, A-1, 1)

  age_idx_std <- (age_idx - mean(age_idx))/sd(age_idx)
  diff_idx_std <- (diff_idx - mean(diff_idx))/sd(diff_idx)

  stan_data$age_idx_std <- age_idx_std
  stan_data$diff_idx_std <- diff_idx_std

  return(stan_data)
}

# Add HSGP parameters
add_hsgp_parms <- function(stan_data, C, M1, M2){
  stan_data$C1 = C
  stan_data$C2 = C
  stan_data$M1 = M1
  stan_data$M2 = M2

  return(stan_data)
}


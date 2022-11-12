#' Create a template stan data object
#'
#' @param A Number of participant age groups
#' @param C Number of contact age groups
#' @param T Number of waves (default NULL)
#'
#' @return list
#' @export
#'
#' @examples stan_data <- init_stan_data()
init_stan_data <- function(A = 85, C = 13, T = NULL){
  if(is.null(T)){ # Single waves
    list(A = A, C = C)
  } else { # Multiple waves
    list( T = T, U = sum(seq(1,T)), R = T, A = A, C = C )
  }
}

#' Makes a age x age strata grid
#'
#' @param A Number of participant age groups
#' @param U wave x repeat index
#' @param gender Whether to include gender columns
#'
#' @return a data.table (grid)
#'
#' @examples g <- make_grid(85, U = 3, gender = TRUE)
make_grid <- function(A, U = NULL, gender = FALSE){
  age_strata <- c("0-4","5-9","10-14","15-19","20-24","25-34","35-44",
                  "45-54","55-64","65-69","70-74","75-79","80-84")

  if(is.null(U)){ # Single wave
    if(gender){
      g <- as.data.table(expand.grid(age = seq(0,A-1),
                                     alter_age_strata = age_strata,
                                     gender = c("Male", "Female"),
                                     alter_gender = c("Male", "Female")))
    } else {

      g <- as.data.table(expand.grid(age = seq(0, A-1),
                                     alter_age_strata = age_strata))
    }
  } else { # Multiple waves
    if(gender){
      g <- as.data.table(expand.grid(u = seq(1,U),
                                     age = seq(0,A-1),
                                     alter_age_strata = age_strata,
                                     gender = c("Male", "Female"),
                                     alter_gender = c("Male", "Female")))
    } else {
      g <- as.data.table(expand.grid(u = seq(1,U),
                                     age = seq(0,A-1),
                                     alter_age_strata = age_strata))
    }
  }

  return(g)
}

add_contact_vector <- function(stan_data, contacts, single=FALSE, survey="COVIMOD"){
  if(survey == "COVIMOD"){
    if(!single){
      d <- contacts[order(u, age, alter_age_strata, gender, alter_gender)]

      stan_data$Y_MM <- d[gender == "Male" & alter_gender == "Male"]$y
      stan_data$Y_FF <- d[gender == "Female" & alter_gender == "Female"]$y
      stan_data$Y_MF <- d[gender == "Male" & alter_gender == "Female"]$y
      stan_data$Y_FM <- d[gender == "Female" & alter_gender == "Male"]$y

      return(stan_data)
    } else { # Single wave
      d <- contacts[order(age, alter_age_strata, gender, alter_gender)]

      stan_data$Y_MM <- d[gender == "Male" & alter_gender == "Male"]$y
      stan_data$Y_FF <- d[gender == "Female" & alter_gender == "Female"]$y
      stan_data$Y_MF <- d[gender == "Male" & alter_gender == "Female"]$y
      stan_data$Y_FM <- d[gender == "Female" & alter_gender == "Male"]$y

      return(stan_data)
    }
  }

  if(survey == "POLYMOD"){
    d <- contacts[order(age, alter_age, gender, alter_gender)]

    stan_data$Y_MM <- d[gender == "Male" & alter_gender == "Male"]$y
    stan_data$Y_FF <- d[gender == "Female" & alter_gender == "Female"]$y
    stan_data$Y_MF <- d[gender == "Male" & alter_gender == "Female"]$y
    stan_data$Y_FM <- d[gender == "Female" & alter_gender == "Male"]$y

    return(stan_data)
  }
}

# Add obs count
add_N <- function(stan_data, survey = "COVIMOD"){

  if(survey == "COVIMOD"){
    stan_data$N_M <- length(stan_data$Y_MM)
    stan_data$N_F <- length(stan_data$Y_FF)
  }

  if(survey == "POLYMOD"){
    stan_data$N_MM <- length(stan_data$Y_MM)
    stan_data$N_FF <- length(stan_data$Y_FF)
    stan_data$N_MF <- length(stan_data$Y_MF)
    stan_data$N_FM <- length(stan_data$Y_FM)
  }

  return(stan_data)
}

add_row_major_idx <- function(stan_data, contacts, survey = "COVIMOD"){
  if (survey == "COVIMOD"){
    d <- contacts[order(u, age, alter_age_strata, gender, alter_gender)]

    d[, age_idx := age + 1]
    d[, age_strata_idx := as.numeric(alter_age_strata)]
    d[, row_major_idx := (age_idx-1)*13 + age_strata_idx]

    stan_data$ROW_MAJOR_IDX_M <- d[gender == "Male" & alter_gender == "Male"]$row_major_idx
    stan_data$ROW_MAJOR_IDX_F <- d[gender == "Female" & alter_gender == "Female"]$row_major_idx

    return(stan_data)
  }

  if (survey == "POLYMOD"){
    d <- contacts[order(age, alter_age, gender, alter_gender)]

    d[, age_idx := age + 1]
    d[, age_strata_idx := alter_age + 1]
    d[, row_major_idx := (age_idx-1)*85 + age_strata_idx]

    stan_data$ROW_MAJOR_IDX_MM <- d[gender == "Male" & alter_gender == "Male"]$row_major_idx
    stan_data$ROW_MAJOR_IDX_FF <- d[gender == "Female" & alter_gender == "Female"]$row_major_idx
    stan_data$ROW_MAJOR_IDX_MF <- d[gender == "Male" & alter_gender == "Female"]$row_major_idx
    stan_data$ROW_MAJOR_IDX_FM <- d[gender == "Female" & alter_gender == "Male"]$row_major_idx

    return(stan_data)
  }
}


# Add start end idx
make_start_end_idx <- function(contacts, .gender){
  d <- contacts[gender == .gender & alter_gender == .gender]

  g <- as.data.table(expand.grid(u = 1:max(contacts$u)))
  d[, idx := 1:.N]
  d <- d[, .(start_idx = min(idx), end_idx = max(idx)), by="u"]

  d <- merge(g, d, by = "u", all.x = TRUE)
  d[is.na(start_idx), start_idx := 0]
  d[is.na(end_idx), end_idx := 0]

  start_idx <- d$start_idx
  end_idx <- d$end_idx

  return(list(start_idx, end_idx))
}

add_start_end_idx <- function(stan_data, contacts){
  start_end_M <- make_start_end_idx(contacts, "Male")
  start_end_F <- make_start_end_idx(contacts, "Female")

  stan_data$START_IDX_M <- start_end_M[[1]]
  stan_data$START_IDX_F <- start_end_F[[1]]

  stan_data$END_IDX_M <- start_end_M[[2]]
  stan_data$END_IDX_F <- start_end_F[[2]]

  return(stan_data)
}

# Add participant size offsets
make_partsize_offsets <- function(offsets, .gender, U, A, survey = "COVIMOD"){
  if (survey == "COVIMOD"){
    d <- offsets[gender == .gender]
    g <- make_grid(A, U)

    d <- merge(g, d, by=c("u", "age"), all.x = TRUE)
    d[is.na(N), N := 1]
    d <- unique(d[,.(u, age, N)])

    d <- data.table::dcast(d, u ~ age, value.var = "N")[,-c("u")]

    return(log(as.matrix(d)))
  }

  if (survey == "POLYMOD"){
    d <- offsets[gender == .gender]
    g <- expand.grid(age = 0:84, alter_age = 0:84)

    d <- as.data.table( merge(g, d, by=c("age", "alter_age"), all.x = TRUE) )
    d[is.na(N), N := 1]
    d <- unique(d[, .(age, N)])

    return(log(d$N))
  }
}

add_partsize_offsets <- function(stan_data, offsets, survey = "COVIMOD"){
  U <- stan_data$U
  A <- stan_data$A

  if (survey == "COVIMOD"){
    stan_data$log_N_M <- make_partsize_offsets(offsets, "Male", U, A)
    stan_data$log_N_F <- make_partsize_offsets(offsets, "Female", U, A)
  }

  if (survey == "POLYMOD"){
    stan_data$log_N_M <- make_partsize_offsets(offsets, "Male", U, A, "POLYMOD")
    stan_data$log_N_F <- make_partsize_offsets(offsets, "Female", U, A, "POLYMOD")
  }

  return(stan_data)
}

add_part_offsets <- function(stan_data, contacts, offsets = NULL, survey = "COVIMOD"){
  d <- contacts

  if(survey == "COVIMOD"){
    stan_data$part_M <- d[gender == "Male" & alter_gender == "Male"]$N
    stan_data$part_F <- d[gender == "Female" & alter_gender == "Female"]$N

    stan_data$S_M <- d[gender == "Male" & alter_gender == "Male"]$zeta
    stan_data$S_F <- d[gender == "Female" & alter_gender == "Female"]$zeta
  }

  if(survey == "POLYMOD"){
    d <- offsets

    stan_data$log_N_M <- log( d[gender == "Male"]$N )
    stan_data$log_N_F <- log( d[gender == "Female"]$N )

    stan_data$log_S_M <- log( d[gender == "Male"]$zeta )
    stan_data$log_S_F <- log( d[gender == "Female"]$zeta )
  }

  return(stan_data)
}

# Add zeta
make_zeta_offsets <- function(offsets, .gender, U, A){
  d <- offsets[gender == .gender]
  g <- make_grid(U, A)

  d <- merge(g, d, by=c("u", "age"), all.x=T)
  d[is.na(N), zeta := 1]
  d <- unique(d[,.(u, age, zeta)])
  d <- data.table::dcast(d, u ~ age, value.var = "zeta")[,-c("u")]

  return(log(as.matrix(d)))
}

add_zeta_offsets <- function(stan_data, offsets){
  U <- stan_data$U
  A <- stan_data$A

  stan_data$log_S_M <- make_zeta_offsets(offsets, "Male", U, A)
  stan_data$log_S_F <- make_zeta_offsets(offsets, "Female", U, A)

  return(stan_data)
}

# Add population offsets
add_pop_offsets <- function(stan_data, pop, survey = "COVIMOD"){
  A <- stan_data$A

  if(survey == "COVIMOD"){
    stan_data$pop_M <- dt.pop[gender == "Male" & age < A]$pop
    stan_data$pop_F <- dt.pop[gender == "Female" & age < A]$pop
  }

  if(survey == "POLYMOD"){
    stan_data$log_P_M <- log( dt.pop[gender == "Male" & age < A]$pop )
    stan_data$log_P_F <- log( dt.pop[gender == "Female" & age < A]$pop )
  }

  return(stan_data)
}

add_missing_u <- function(stan_data, u.missing){
  U <- stan_data$U

  missing_u <- rep(0, U)
  for(u in 1:U){
    if(u %in% u.missing) missing_u[u] <- 1
  }

  stan_data$missing_u <- missing_u # I'm missing U (haha)
  return(stan_data)
}

# Map U to T
add_map_u_to_t <- function(stan_data){
  U <- stan_data$U
  T <- stan_data$T

  map_u_to_t <- c()
  for (t in 1:T) map_u_to_t <- c(map_u_to_t, rep(t, t))

  stan_data$map_u_to_t <- map_u_to_t
  return(stan_data)
}

# Map U to R
add_map_u_to_r <- function(stan_data){
  T <- stan_data$T
  R <- stan_data$R

  map_u_to_r <- c()
  for (t in 1:T) map_u_to_r <- c(map_u_to_r, seq(1, t))

  stan_data$map_u_to_r <- map_u_to_r
  return(stan_data)
}

# Add indicator matrix for t
add_delta_t <- function(stan_data){
  T <- stan_data$T
  delta_t <- diag(1, T-1, T-1)
  delta_t <- rbind(rep(0,T-1), delta_t)

  stan_data$delta_t <- delta_t
  return(stan_data)
}

# Add indicator matrix for r
add_delta_r <- function(stan_data){
  R <- stan_data$R

  delta_r <- diag(1, R-1, R-1)
  delta_r <- rbind(rep(0,R-1), delta_r)

  stan_data$delta_r <- delta_r
  return(stan_data)
}

add_map_tr_to_u <- function(stan_data){
  U <- stan_data$U
  T <- stan_data$T
  R <- stan_data$R

  lower_tri_idx <- rep(0, (T*(T+1))%/%2)
  tmp_int = 1
  for (t in 1:T){
    for (r in 1:t){
      lower_tri_idx[tmp_int] = T*(t-1) + r;
      tmp_int = tmp_int + 1;
    }
  }

  tmp <- rep(0, T*R)
  tmp[lower_tri_idx] <- seq(1:U)

  map_tr_to_u <- matrix(tmp, nrow=T, ncol=R, byrow = T)

  stan_data$map_tr_to_u <- map_tr_to_u

  return(stan_data)
}

# Map age to age strata
add_map_age_to_strata <- function(stan_data, survey = "COVIMOD"){
  if (survey == "COVIMOD"){
    age_strata <- c("0-4","5-9","10-14","15-19","20-24","25-34","35-44",
                    "45-54","55-64","65-69","70-74","75-79","80-84")

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

  if (survey == "POLYMOD"){
    map_age_to_strata <- diag(rep(1, stan_data$A))

    stan_data$map_age_to_strata <- map_age_to_strata

    return(stan_data)
  }
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


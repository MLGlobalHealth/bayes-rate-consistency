make_stan_data <- function(A,
                           C,
                           dt_contacts,
                           dt_offsets,
                           dt_population,
                           model_params,
                           single_wave = TRUE,
                           waves = NULL,
                           single_contact_age = FALSE){
  if (single_wave) {

    stan_data <- init_stan_data(A, C)
    stan_data <- add_participant_offsets(stan_data,
                                         dt_contacts,
                                         dt_offsets)
    stan_data <- add_population_offsets(stan_data, dt_population)

    if (!single_contact_age) {

      stan_data <- add_contact_vector(stan_data, dt_contacts)
      stan_data <- add_row_major_idx(stan_data, dt_contacts)
      stan_data <- add_age_strata_map(stan_data)
    } else {

      stan_data <- add_contact_vector(stan_data,
                                      dt_contacts,
                                      single_contact_age = TRUE)
      stan_data <- add_row_major_idx(stan_data,
                                     dt_contacts,
                                     single_contact_age = TRUE)
      stan_data <- add_age_strata_map(stan_data, single_contact_age = TRUE)
    }

    stan_data <- add_N(stan_data, single_wave = TRUE)

  } else {

    stan_data <- init_stan_data(A, C, waves)
    stan_data <- add_contact_vector(stan_data,
                                    dt_contacts,
                                    single_wave = FALSE)
    stan_data <- add_N(stan_data, single_wave = FALSE)
    stan_data <- add_row_major_idx(stan_data, dt_contacts)
    stan_data <- add_start_end_idx(stan_data, dt_contacts)
    stan_data <- add_participant_offsets(stan_data,
                                         dt_contacts,
                                         single_wave = FALSE)
    stan_data <- add_population_offsets(stan_data,
                                        dt_population,
                                        single_wave = FALSE)
    stan_data <- add_wave_repeat_u_map(stan_data)
    stan_data <- add_age_strata_map(stan_data)
  }

  stan_data <- add_non_nuisance_idx(stan_data)
  stan_data <- add_std_age_idx(stan_data)
  stan_data <- add_hsgp_parms(stan_data,
                              model_params$hsgp_binf,
                              model_params$hsgp_m1,
                              model_params$hsgp_m2)

  return(stan_data)
}

#' Create a template stan data object
#'
#' @param A Number of participant age groups
#' @param C Number of contact age groups
#' @param W Number of waves (default NULL)
#'
#' @return list
#' @export
#'
#' @examples stan_data <- init_stan_data()
init_stan_data <- function(A = 85, C = 13, W = NULL) {
  if (is.null(W)) {
    # Single waves
    return(list(A = A, C = C))
  } else {
    # Multiple waves
    U <- sum(seq(1, W))
    return(list(W = W, U = U, R = W, A = A, C = C))
  }
}

#' Generate a data frame with a grid of demographic groups
#'
#' This function generates a data.table with demographic groups that can be used as input
#' for a model. The data frame includes columns for age, age strata, and
#' optionally gender.
#'
#' @param A The maximum age in the model
#' @param U The number of time periods in the model
#' @param gender A logical value indicating whether to include gender in the output
#' @return A data table with demographic groups
#' @examples
#' make_grid(85, 2)
#' make_grid(85, 2, gender = TRUE)
make_grid <- function(A, U = NULL, gender = FALSE) {
  age_strata <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-34", "35-44",
                  "45-54", "55-64", "65-69", "70-74", "75-79", "80-84")

  if (is.null(U)) { # Single wave
    if (gender) {
      g <- expand.grid(age = seq(0, A - 1),
                       alter_age_strata = age_strata,
                       gender = c("Male", "Female"),
                       alter_gender = c("Male", "Female"))
    } else {
      g <- expand.grid(age = seq(0, A - 1),
                       alter_age_strata = age_strata)
    }
  } else { # Multiple waves
    if (gender) {
      g <- expand.grid(u = seq(1, U),
                       age = seq(0, A - 1),
                       alter_age_strata = age_strata,
                       gender = c("Male", "Female"),
                       alter_gender = c("Male", "Female"))
    } else {
      g <- expand.grid(u = seq(1, U),
                       age = seq(0, A - 1),
                       alter_age_strata = age_strata)
    }
  }

  return(data.table(g))
}

#' Add contact count vectors to the Stan data object
#'
#' This function adds contact count vectors to the Stan data object.
#' The contact vectors are generated from a data.table object of contact
#' data, which must contain columns for the age of the individuals, the age strata of
#' the contact, the gender of the individuals, the gender of the contact, and the
#' number of contacts between the individuals and the contacts. The function can
#' handle contact data from either the COVIMOD or POLYMOD surveys, and can add
#' contact vectors for single or multiple waves.
#'
#' @param stan_data A list or data.frame object containing the parameters required
#'                  for the contact model.
#' @param dt_contacts A data.table object containing the contact count data to be used to
#'                    generate the contact count vectors.
#' @param single_wave A logical value indicating whether the contact count data is from a
#'                    single wave survey (default: \code{TRUE}).
#' @param single_contact_age A logical value indicating whether the age of contact is in one year
#'                           intervals or aggregated into age groups (default: \code{FALSE})
#'
#' @return The Stan data object with contact count vectors added to it
add_contact_vector <- function(stan_data,
                               dt_contacts,
                               single_wave = TRUE,
                               single_contact_age = FALSE){
  if (single_wave) {
    if (!single_contact_age) {
      sort_order <- c("age", "alter_age_strata", "gender", "alter_gender")
    } else {
      sort_order <- c("age", "alter_age", "gender", "alter_gender")
    }
  } else {
    sort_order <- c("u", "age", "alter_age_strata", "gender", "alter_gender")
  }

  dt <- dt_contacts[, .(y), keyby = sort_order]

  stan_data$Y_MM <- dt[gender == "Male" & alter_gender == "Male", y]
  stan_data$Y_FF <- dt[gender == "Female" & alter_gender == "Female", y]
  stan_data$Y_MF <- dt[gender == "Male" & alter_gender == "Female", y]
  stan_data$Y_FM <- dt[gender == "Female" & alter_gender == "Male", y]

  return(stan_data)
}

# Add observation count
add_N <- function(stan_data, single_wave = TRUE){

  if (single_wave) {
    stan_data$N_MM <- length(stan_data$Y_MM)
    stan_data$N_FF <- length(stan_data$Y_FF)
    stan_data$N_MF <- length(stan_data$Y_MF)
    stan_data$N_FM <- length(stan_data$Y_FM)
    
  } else {

    stan_data$N_M <- length(stan_data$Y_MM)
    stan_data$N_F <- length(stan_data$Y_FF)
  }

  return(stan_data)
}

#' Add row-major index to Stan data object
#'
#' Adds a row-major index to the Stan data object based on the contact data in
#' dt_contacts. The index is stored in the Stan data object under the
#' appropriate column name depending on the survey type specified.
#'
#' @param stan_data Stan data object to be modified
#' @param dt_contacts Contact data in a data.table object
#' @param single_wave A logical value indicating whether the contact count data is from a
#'                    single wave survey (default: \code{TRUE}).
#' @param single_contact_age A logical value indicating whether the age of contact is in one year
#'                           intervals or aggregated into age groups (default: \code{FALSE})
#'
#' @return Stan data object with row-major index added
#'
#' @details For "COVIMOD" surveyx, the row-major index is computed using the
#' formula (age_idx - 1) * 13 + age_strata_idx, where age_idx is the age of the
#' contact plus 1 and age_strata_idx is the age stratum of the contact. For
#' "POLYMOD" survey type, the row-major index is computed using the formula
#' (age_idx - 1) * 85 + age_strata_idx, where age_idx is the age of the contact
#' plus 1 and age_strata_idx is the age of the contact plus 1.
#'
#' @examples
#' data(stan_data)
#' data(dt_contacts)
#' stan_data <- add_row_major_idx(stan_data, dt_contacts, survey = "COVIMOD")
#'
#' @importFrom data.table order
#'
#' @export
add_row_major_idx <- function(stan_data,
                              dt_contacts,
                              single_wave = TRUE,
                              single_contact_age = FALSE){
  A <- stan_data$A
  C <- stan_data$C

  if (single_wave) {

    if (!single_contact_age) {

      d <- dt_contacts[order(age, alter_age_strata, gender, alter_gender)]
      d[, age_idx := age + 1]
      d[, age_strata_idx := as.numeric(alter_age_strata)]
      d[, row_major_idx := (age_idx-1)*C + age_strata_idx]

    } else {
      d <- dt_contacts[order(age, alter_age, gender, alter_gender)]
      d[, age_idx := age + 1]
      d[, age_strata_idx := alter_age + 1]
      d[, row_major_idx := (age_idx-1)*C + age_strata_idx]
    }

    stan_data$ROW_MAJOR_IDX_MM <- d[gender == "Male" & alter_gender == "Male"]$row_major_idx
    stan_data$ROW_MAJOR_IDX_FF <- d[gender == "Female" & alter_gender == "Female"]$row_major_idx
    stan_data$ROW_MAJOR_IDX_MF <- d[gender == "Male" & alter_gender == "Female"]$row_major_idx
    stan_data$ROW_MAJOR_IDX_FM <- d[gender == "Female" & alter_gender == "Male"]$row_major_idx

    return(stan_data)
  } else {

    d <- dt_contacts[order(u, age, alter_age_strata, gender, alter_gender)]

    d[, age_idx := age + 1]
    d[, age_strata_idx := as.numeric(alter_age_strata)]
    d[, row_major_idx := (age_idx-1)*C + age_strata_idx]

    stan_data$ROW_MAJOR_IDX_M <- d[gender == "Male" & alter_gender == "Male"]$row_major_idx
    stan_data$ROW_MAJOR_IDX_F <- d[gender == "Female" & alter_gender == "Female"]$row_major_idx

    return(stan_data)
  }
}


#' Add start and end indices to a data frame based on contacts
#'
#' This function adds start and end indices for male and female genders in the given data
#' frame based on the contacts data. The function creates start and end indices for a given
#' gender using the \code{\link{make_start_end_idx}} function.
#'
#' @param stan_data A data frame to which start and end indices are added.
#' @param dt_contacts A data table containing contacts information.
#' @return A modified version of the stan_data with start and end indices added.
#' @export
#'
#' @examples
#' # Load the data and add the start and end indices
#' data(contacts)
#' stan_data <- as.data.frame(contacts)
#' stan_data <- add_start_end_idx(stan_data, contacts)
add_start_end_idx <- function(stan_data, dt_contacts){
  # function to create start and end indices for a given gender
  make_start_end_idx <- function(dt_contacts, gender) {
    # subset the data.table
    d <- dt_contacts[gender == gender & alter_gender == gender]

    # create a data.table with all possible 'u' values
    g <- data.table(u = 1:max(dt_contacts$u))

    # add an index column to the subset and calculate the start and end indices
    d[, idx := 1:.N]
    d <- d[, .(start_idx = min(idx), end_idx = max(idx)), by = "u"]

    # merge with the data.table with all possible 'u' values and replace NAs with 0
    d <- merge(g, d, by = "u", all.x = TRUE)
    d[is.na(start_idx), start_idx := 0]
    d[is.na(end_idx), end_idx := 0]

    # extract and return start_idx and end_idx as a list
    list(start_idx = d$start_idx, end_idx = d$end_idx)
  }

  # create start and end indices for Male and Female separately
  start_end_M <- make_start_end_idx(dt_contacts, "Male")
  start_end_F <- make_start_end_idx(dt_contacts, "Female")

  # add the start and end indices to the stan_data object
  stan_data$START_IDX_M <- start_end_M$start_idx
  stan_data$START_IDX_F <- start_end_F$start_idx
  stan_data$END_IDX_M <- start_end_M$end_idx
  stan_data$END_IDX_F <- start_end_F$end_idx

  return(stan_data)
}

#' Add Participant Offsets to Stan Data Object
#'
#' This function adds participant offsets to a Stan data object based on the survey type and data tables containing information about the contacts and participants.
#'
#' @param stan_data A Stan data object
#' @param dt_contacts A data table containing information about the contacts
#' @param dt_offsets A data table containing information about the participants (optional, defaults to NULL)
#' @param single_wave A logical value indicating whether the contact count data is from a
#'                    single wave survey (default: \code{TRUE}).
#'
#' @return A Stan data object with participant offsets added
#'
#' @examples
#' add_part_offsets(stan_data, dt_contacts)
#' add_part_offsets(stan_data, dt_contacts, dt_offsets, survey = "POLYMOD")
add_participant_offsets <- function(stan_data,
                             dt_contacts,
                             dt_offsets = NULL,
                             single_wave = TRUE){

  if (single_wave) {
    # get the log of N and S values for males and females
    d_M <- complete(dt_offsets[gender == "Male"], age = 0:84, fill = list(N = 1, S = 1))
    d_F <- complete(dt_offsets[gender == "Female"], age = 0:84, fill = list(N = 1, S = 1))

    stan_data$log_N_M <- log(d_M$N)
    stan_data$log_N_F <- log(d_F$N)

    stan_data$log_S_M <- log(d_M$S)
    stan_data$log_S_F <- log(d_F$S)
  } else {
    # get the number of participants and S values for males and females
    d_M <- dt_contacts[gender == "Male" & alter_gender == "Male"]
    d_F <- dt_contacts[gender == "Female" & alter_gender == "Female"]

    stan_data$part_M <- d_M$N
    stan_data$part_F <- d_F$N

    stan_data$S_M <- d_M$S
    stan_data$S_F <- d_F$S
  }

  return(stan_data)
}

#' Add population offsets to Stan data
#'
#' Adds population offsets to Stan data for use in the Stan model. Population
#' offsets adjust for differences in population sizes between age groups and
#' genders.
#'
#' @param stan_data The data in the format expected by the Stan model
#' @param dt_population A data.table containing population sizes by gender and age
#' @param single_wave A logical value indicating whether the contact count data is from a
#'                    single wave survey (default: \code{TRUE}).
#'
#' @return The modified Stan data with population offsets added
add_population_offsets <- function(stan_data,
                                   dt_population,
                                   single_wave = TRUE){
  A <- stan_data$A

  if (single_wave) {

    stan_data$log_P_M <- log(dt_population[gender == "Male" & age < A]$pop)
    stan_data$log_P_F <- log(dt_population[gender == "Female" & age < A]$pop)
  } else {

    stan_data$pop_M <- dt_population[gender == "Male" & age < A]$pop
    stan_data$pop_F <- dt_population[gender == "Female" & age < A]$pop
  }

  return(stan_data)
}

#' Add wave-repeat to combined index map to Stan data
#'
#' Given the number of waves (W) and the number of repeated participation (R),
#' create a map from the wave and repeat index to a combined index (U).
#' This optimization to improve Stan fitting time.
#'
#' @param stan_data A list containing Stan data
#'
#' @return The modified Stan data with a new variable 'map_tr_to_u'
#'
#' @examples
#' stan_data <- list(U = 100, W = 4, R = 3)
#' stan_data <- add_wave_repeat_u_map(stan_data)
add_wave_repeat_u_map <- function(stan_data){

  # Extract variables from stan_data
  U <- stan_data$U # Wave and repeat combined index
  W <- stan_data$W # Number of waves
  R <- stan_data$R # Number of repeated participation

  lower_tri_idx <- rep(0, (W * (W + 1)) %/% 2)
  tmp_int = 1
  for (w in 1:W){
    for (r in 1:w){
      lower_tri_idx[tmp_int] = W * (w - 1) + r;
      tmp_int = tmp_int + 1;
    }
  }

  # Create map from lower triangular indices
  tmp <- rep(0, W*R)
  tmp[lower_tri_idx] <- seq(1:U)
  map_tr_to_u <- matrix(tmp, nrow = W, ncol = R, byrow = TRUE)

  stan_data$map_tr_to_u <- map_tr_to_u
  return(stan_data)
}

#' Map age to age strata
#'
#' This function maps age to age strata for a given survey and adds the resulting
#' map to the provided stan_data object. For COVIMOD survey, age is mapped to
#' predefined age strata. For POLYMOD survey, age is mapped to itself.
#'
#' @param stan_data A list object containing data for the Stan model.
#' @param dt_contacts A data table containing contacts information.
#' @param single_wave A logical value indicating whether the contact count data is from a
#'                    single wave survey (default: \code{TRUE}).
#'
#' @return The modified stan_data object with the age-to-strata map added.
#' @examples
#' data(stan_data)
#' stan_data <- add_age_strata_map(stan_data, survey = "COVIMOD")
#' stan_data <- add_age_strata_map(stan_data, survey = "POLYMOD")
add_age_strata_map <- function(stan_data,
                               dt_contacts,
                               single_contact_age = FALSE){

  if (!single_contact_age) {

    # define age strata for COVIMOD survey
    age_strata <- sort(unique(dt_contacts$alter_age_strata))
    if (length(age_strata) != stan_data$C) { # Handle edge cases
      stop("The number of age stratum does not match the number of age stratum detected in the data!")
    }

    # extract minimum age for each strata
    alter_age_min <- as.numeric(str_extract(as.character(age_strata), "^[0-9]{1,2}"))

    strata_min <- unique(alter_age_min)
    strata_min_idx <- strata_min - min(strata_min) + 1

    # get dimensions of data
    A <- stan_data$A
    C <- stan_data$C

    # create matrix to map age to age strata
    map_age_to_strata <- matrix(0, nrow=A, ncol=C)
    for (c in 1:C) {
      if (c == C) {
        map_age_to_strata[strata_min_idx[c]:A, c] <- 1
      } else {
        map_age_to_strata[strata_min_idx[c]:(strata_min_idx[c+1]-1), c] <- 1
      }
    }

    # add map to Stan_data
    stan_data$map_age_to_strata <- map_age_to_strata
    return(stan_data)

  } else {
    # create identity matrix to map age to age strata for POLYMOD survey
    map_age_to_strata <- diag(rep(1, stan_data$A))

    # add map to Stan_data
    stan_data$map_age_to_strata <- map_age_to_strata
    return(stan_data)
  }
}

#' Add non-nuisance index
#'
#' This function adds a non-nuisance index to the given Stan data.
#'
#' @param stan_data A list containing the Stan data.
#'
#' @return A list containing the updated Stan data with the non-nuisance index added.
#'
#' @examples
#' stan_data <- list(A = 3, B = 2, C = 4)
#' add_non_nuisance_idx(stan_data)
add_non_nuisance_idx <- function(stan_data) {
  A <- stan_data$A

  NN_IDX <- matrix(NA, nrow = A, ncol = A)
  for (i in 1:A) {
    min_idx <- (2*A-1)*(i-1) + (A-i+1)
    max_idx <- (2*A-1)*(i-1) + (2*A-i)
    NN_IDX[i, ] <- seq.int(min_idx, max_idx, 1)
  }

  stan_data$NN_IDX <- sort(as.vector(NN_IDX))
  return(stan_data)
}

#' Add standardized age index
#'
#' Add standardized age index to the input \code{stan_data} list.
#'
#' @param stan_data A list containing data needed for Stan modeling.
#'                  Must contain element \code{A}, which is the maximum age.
#'
#' @return A modified version of the input \code{stan_data} list, with
#'         elements \code{age_idx_std} and \code{diff_idx_std} added.
#'
#' @examples
#' add_std_age_idx(list(A = 100))
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


#' Add HSGP parameters
#'
#' Adds Hilbert Space approximate Gaussian Process (HSGP) parameters to the given Stan data.
#'
#' @param stan_data A list containing the Stan data.
#' @param C A scalar, the value of the HSGP hyperparameter C.
#' @param M1 A scalar, the number of basis functions on the first dimension.
#' @param M2 A scalar, the number of basis functions on the second dimension.
#'
#' @return The updated Stan data as a list.
#'
#' @examples
#' # Load sample data
#' data(stan_data)
#'
#' # Add HSGP parameters
#' stan_data <- add_hsgp_parms(stan_data, C = 1.5, M1 = 30, M2 = 20)
add_hsgp_parms <- function(stan_data, C, M1, M2){
  stan_data$C1 = C
  stan_data$C2 = C
  stan_data$M1 = M1
  stan_data$M2 = M2

  return(stan_data)
}


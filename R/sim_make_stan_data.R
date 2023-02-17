library(stringr)

#' Create stan_data for simulation experiments
#'
#' This function Stan data for simulation experiments to be used with Stan models. It initializes the
#' `stan_data` list with `init_stan_data()`, and adds various parameters and offsets
#' using other functions:
#' - `add_N()`
#' - `add_contacts()`
#' - `add_row_major_idx()`
#' - `add_partsize_offsets()`
#' - `add_group_offsets()`
#' - `add_pop_offsets()`
#' - `add_age_strata_map()`
#' - `add_std_age_idx()`
#' - `add_non_nuisance_idx()` (if model name contains "rd")
#' - `add_hsgp_parms()` (if model name contains "hsgp")
#'
#' @param data a data.table containing the simulated data
#' @param A an integer indicating the number of age strata
#' @param strata.scheme a character string indicating the name of the age strata scheme
#' @param model.name a character string indicating the name of the Stan model to be used
#'
#' @return a list containing the simulated data for use with Stan models
#' @export
#'
#' @examples
#' data <- simulate_data()
#' stan_data <- make_sim_stan_data(data, 44, "COVIMOD", model_params)
sim_make_stan_data <- function(data, A, strata.scheme, model.params){
  stan_data <- init_stan_data(A, strata.scheme)
  stan_data <- add_N(stan_data, data)
  stan_data <- add_contacts(stan_data, data)
  stan_data <- add_row_major_idx(stan_data, data)
  stan_data <- add_partsize_offsets(stan_data, data)
  stan_data <- add_group_offsets(stan_data)
  stan_data <- add_pop_offsets(stan_data, data)
  stan_data <- add_age_strata_map(stan_data, strata.scheme)
  stan_data <- add_std_age_idx(stan_data)

  if (stringr::str_detect(model.params$name, "rd")){
    stan_data <- add_non_nuisance_idx(stan_data)
  }

  if (stringr::str_detect(model.params$name, "hsgp")){
    stan_data <- add_hsgp_parms(stan_data,
                                model.params$hsgp_binf,
                                model.params$hsgp_m1,
                                model.params$hsgp_m2)
  }

  return(stan_data)
}

#' Initialize Stan data
#'
#' Returns a list containing initialized Stan data.
#'
#' @param A integer, number of age groups
#' @param strata.scheme character, strata scheme, one of "COVIMOD", "CoMix", "5yr", or "3yr"
#'
#' @return list containing initialized Stan data
#'
#' @examples
#' init_stan_data(10, "5yr")
init_stan_data <- function(A, strata.scheme){
  strata_config <- list(
    "COVIMOD" = 7,
    "CoMix" = 5,
    "5yr" = 9,
    "3yr" = 15
  )

  list(
    A = A,
    C = strata_config[[strata.scheme]]
  )
}

#' Add N counts to the stan_data object
#'
#' This function calculates the total number of unique participants in each gender combination (MM, FF, MF, and FM) and adds the result as a new variable to the input \code{stan_data} object.
#'
#' @param stan_data The stan_data object to add the counts to.
#' @param data The data object containing participant information.
#'
#' @return The modified \code{stan_data} object with added variables for each gender combination count.
#'
#' @examples
#' \dontrun{
#' # create the stan_data object
#' stan_data <- init_stan_data(10, "5yr")
#'
#' # add N counts
#' stan_data <- add_N(stan_data, data)
#' }
add_N <- function(stan_data, data){
  data <- unique(data[part > 0, .(age, gender, alter_age_strata, alter_gender)])

  stan_data$N_MM <- nrow(data[gender == "Male" & alter_gender == "Male"])
  stan_data$N_FF <- nrow(data[gender == "Female" & alter_gender == "Female"])
  stan_data$N_MF <- nrow(data[gender == "Male" & alter_gender == "Female"])
  stan_data$N_FM <- nrow(data[gender == "Female" & alter_gender == "Male"])

  return(stan_data)
}

#' Add contact count vectors to stan_data
#'
#' This function adds the total contact count vectors to the \code{stan_data}
#' for each gender pair.
#'
#' @param stan_data A list object containing data required for a Stan model.
#' @param data A data table containing population level data.
#'
#' @return The modified \code{stan_data} object with the added contact count vectors.
#'
#' @examples
#' \dontrun{
#' stan_data <- init_stan_data(A = 20, strata.scheme = "COVIMOD")
#' data <- read.csv("my_data.csv")
#' stan_data <- add_contacts(stan_data, data)
#' }
#'
#' @export
add_contacts <- function(stan_data, data){
  data <- unique(data[, list(age, gender, alter_age_strata, alter_gender, y_strata)])
  data <- data[order(age, alter_age_strata)]

  stan_data$Y_MM <- data[gender == "Male" & alter_gender == "Male"]$y_strata
  stan_data$Y_FF <- data[gender == "Female" & alter_gender == "Female"]$y_strata
  stan_data$Y_MF <- data[gender == "Male" & alter_gender == "Female"]$y_strata
  stan_data$Y_FM <- data[gender == "Female" & alter_gender ==  "Male"]$y_strata

  return(stan_data)
}

#' Add row major index to Stan data
#'
#' Given a data.table `data` containing the columns "age", "gender", "alter_age_strata",
#' "alter_gender", "part", and "y_strata", this function adds row major indices to
#' four corresponding variables in the Stan data object `stan_data`:
#' "ROW_MAJOR_IDX_MM", "ROW_MAJOR_IDX_FF", "ROW_MAJOR_IDX_MF", and "ROW_MAJOR_IDX_FM".
#'
#' @param stan_data A list containing the necessary components for the Stan model
#' @param data A data.table containing the columns "age", "gender", "alter_age_strata",
#'   "alter_gender", "part", and "y_strata"
#'
#' @return The modified `stan_data` list with row major indices added
#'
#' @examples
#' data(example_data)
#' stan_data <- add_row_major_idx(stan_data, example_data)
#' stan_fit <- stan(model_code = stan_code, data = stan_data, chains = 4)
#'
#' @export
add_row_major_idx <- function(stan_data, data) {
  make_row_major_idx <- function(data, C) {
    data <- unique(data[, .(age, alter_age_strata, part)])
    data[, age_idx := age - 5]
    data[, alter_age_strata_idx := as.numeric(alter_age_strata)]
    data[, row_major_idx := (age_idx - 1) * C + alter_age_strata_idx]
    data <- data[part > 0]
    data <- data[order(age_idx, alter_age_strata_idx)]
    return(data$row_major_idx)
  }

  C <- stan_data$C
  gender_pairs <- list(
    c(gender = "Male", alter_gender = "Male"),
    c(gender = "Female", alter_gender = "Female"),
    c(gender = "Male", alter_gender = "Female"),
    c(gender = "Female", alter_gender = "Male")
  )
  for (pair in gender_pairs) {
    gender1 <- pair[1]
    gender2 <- pair[2]
    tmp <- data[gender == gender1 & alter_gender == gender2]
    row_major_idx <- make_row_major_idx(tmp, C)
    stan_data[[paste0("ROW_MAJOR_IDX_", substr(gender1, 1, 1), substr(gender2, 1, 1))]] <- row_major_idx
  }

  return(stan_data)
}

#' Add Participant Size Offsets to Stan Data
#'
#' This function computes and adds the participant size offsets to the input Stan
#' data frame. The participant size offsets are the natural logarithm of the
#' number of participants for each gender group. This is used to account for the
#' varying number of participants in each group when fitting models in Stan.
#'
#' @param stan_data A list object containing the Stan data frame.
#' @param data A data frame containing the data.
#'
#' @return A list object containing the updated Stan data frame with the
#'   participant size offsets added.
#'
#' @examples
#' # Add participant size offsets to Stan data
#' stan_data <- add_partsize_offsets(stan_data, data)
add_partsize_offsets <- function(stan_data, data){
  data <- unique(data[, list(age, gender, part)])
  data[part == 0, part := 1]

  stan_data$log_N_M <- log(data[gender == "Male"]$part)
  stan_data$log_N_F <- log(data[gender == "Female"]$part)

  return(stan_data)
}

# Add group offsets (For simulated data we won't consider group offsets)
add_group_offsets <- function(stan_data){
  A <- stan_data$A

  stan_data$log_S_M <- rep(0,A)
  stan_data$log_S_F <- rep(0,A)

  return(stan_data)
}

#' Add population offsets to Stan data.
#'
#' This function calculates the log of the population counts for male and female
#' alters and adds them to the Stan data object.
#'
#' @param stan_data A Stan data object to add the population offsets to.
#' @param data A data frame with the population counts for each age-gender group.
#'
#' @return A Stan data object with added log population offsets for male and female alters.
#'
#' @examples
#' data <- data.frame(alter_age = c(25, 30, 35),
#'                    alter_gender = c("Male", "Male", "Female"),
#'                    pop = c(1000, 1500, 2000))
#' stan_data <- list()
#' add_pop_offsets(stan_data, data)
add_pop_offsets <- function(stan_data, data){
  data <- unique(data[, list(alter_age, alter_gender, pop)])
  data <- data[order(alter_age)]

  stan_data$log_P_M <- log(data[alter_gender == "Male"]$pop)
  stan_data$log_P_F <- log(data[alter_gender == "Female"]$pop)

  return(stan_data)
}

#' Add Age-Stratum Mapping to Stan Data
#'
#' This function adds an age-stratum mapping matrix to the provided Stan data,
#' based on a specified stratum scheme.
#'
#' @param stan_data a named list containing data for a Stan model, with elements:
#'   - A: integer, the number of age groups
#'   - C: integer, the number of strata
#'   The list can also contain other elements for the model.
#' @param strata.scheme a string indicating the stratum scheme to use. Available options are:
#'   - "COVIMOD": 6-9, 10-14, 15-19, 20-24, 25-34, 35-44, 45-49
#'   - "CoMix": 6-11, 12-17, 18-29, 30-39, 40-49
#'   - "5yr": 6-9, 10-14, 15-19, 20-24, 25-29, 30-34, 35-39, 40-44, 45-49
#'   - "3yr": 6-8, 9-11, 12-14, 15-17, 18-20, 21-23, 24-26, 27-29,
#'     30-32, 33-35, 36-38, 39-41, 42-44, 45-47, 48-49
#'
#' @return a named list containing the original Stan data, with the following new element:
#'   - map_age_to_strata: matrix, an A x C matrix (A: number of age groups, C: number of strata)
#'     representing the mapping of each age group to a stratum according to the specified stratum scheme.
#'
#' @examples
#' stan_data <- list(A = 10, C = 4)
#' stan_data <- add_age_stratum_mapping(stan_data, strata.scheme = "COVIMOD")
#' str(data_with_mapping$map_age_to_strata)
#'
#' @export
add_age_strata_map <- function(stan_data, strata.scheme) {
  lookup_table <- list(
    "COVIMOD" = c("6-9", "10-14", "15-19", "20-24", "25-34", "35-44", "45-49"),
    "CoMix" = c("6-11", "12-17", "18-29", "30-39", "40-49"),
    "5yr" = c("6-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"),
    "3yr" = c("6-8", "9-11", "12-14", "15-17", "18-20", "21-23", "24-26", "27-29",
              "30-32", "33-35", "36-38", "39-41", "42-44", "45-47", "48-49")
  )

  # Extract age strata based on selected strata scheme
  age_strata <- lookup_table[[strata.scheme]]

  # Extract the minimum age of alter_age
  alter_age_min <- as.numeric(str_extract(as.character(age_strata), "^[0-9]{1,2}"))

  # Extract the minimum age of each stratum and its corresponding index
  strata_min <- unique(alter_age_min)
  strata_min_idx <- strata_min - min(strata_min) + 1

  # Extract A and C from the stan_data object
  A <- stan_data$A
  C <- stan_data$C

  # Initialize map_age_to_strata as a matrix of 0's
  map_age_to_strata <- matrix(0, nrow = A, ncol = C)

  # Create map_age_to_strata by assigning 1's to the appropriate age ranges for each stratum
  for (c in 1:C) {
    if (c == C) {
      map_age_to_strata[strata_min_idx[c]:A, c] <- 1
    } else {
      map_age_to_strata[strata_min_idx[c]:(strata_min_idx[c + 1] - 1), c] <- 1
    }
  }

  # Add the map_age_to_strata to the stan_data object
  stan_data$map_age_to_strata <- map_age_to_strata

  # Return the modified stan_data object
  return(stan_data)
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
#'
#' @export
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


#' Impute Missing Child Age Data
#'
#' This function imputes missing age data for child records in a given data
#' table. The age is imputed using the age strata of the record and a uniform
#' distribution within that range.
#'
#' @param dt_participants A data.table object with columns for new_id, wave, age, and
#' age_strata
#' @param seed An optional integer for setting the random seed used in age
#' imputation
#'
#' @return A modified version of the input data.table object with missing ages
#' imputed
#'
#' @examples
#' dt_participants <- data.table(new_id = c(1, 2, 3, 4),
#'                       wave = c(1, 1, 2, 2),
#'                       age = c(2, NA, NA, 5),
#'                       age_strata = c("0-4", "5-9", "5-9", "5-9"))
#' imputed_dt <- impute_child_age(dt_participants)
#'
#' @importFrom stringr str_extract
#' @export
fill_missing_child_ages <- function(dt_participants, seed = 1527) {
  set.seed(seed)
  runif.int <- function(min, max) floor(runif(1, min = min, max = max + 0.999))

  # Create a new data table with unique records and extract age strata info
  unique_dt <- unique(dt_participants, by = c("new_id", "wave", "age_strata"))
  unique_dt[, `:=`(
    min_age = as.numeric(stringr::str_extract(age_strata, "[0-9]{1,2}")),
    max_age = as.numeric(stringr::str_extract(age_strata, "[0-9]{1,2}$"))
  )]

  # Impute missing age using uniform distribution within age strata range
  unique_dt[is.na(age), imp_age := runif.int(min_age, max_age), by = .(wave, new_id)]
  unique_dt[!is.na(age), imp_age := age]

  # Merge imputed age data back into original data table
  dt_participants <- merge(dt_participants, unique_dt[, list(new_id, wave, imp_age)])

  return(dt_participants)
}

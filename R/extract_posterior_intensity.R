#' Extract log contact rates from a posterior draws object
#'
#' This function takes two inputs: 'posterior_draws' and 'dt_population'. 
#' It returns a data.table object that contains posterior draws of log contact rates.
#'
#' @param posterior_draws A matrix of posterior draws, where each row represents a draw and each column represents a different parameter.
#' @param dt_population A data.table object that contains population data, with columns for age, gender, and population size.
#'
#' @return A data.table object that contains the posterior draws of log contact rates, along with additional columns for age, gender, and contact intensities.
#'
#' @examples
#' posterior_draws <- matrix(runif(100), ncol = 4)
#' dt_population <- data.table(age = c(20, 30, 40), gender = c("Male", "Female", "Male"), pop = c(100, 200, 300))
#' result <- extract_posterior_intensity(posterior_draws, dt_population)
#'
#' @importFrom reshape2 melt
#' @importFrom stringr str_extract_all
#' @importFrom data.table as.data.table merge
extract_posterior_intensity <- function(posterior_draws, dt_population){
  posterior_draws <- subset(posterior_draws, "log_cnt_rate")
  dt_posterior <- setDT(reshape2::melt(posterior_draws))

  # Extract indices
  # Note: cannot use str_match as it throws a memory related error
  pattern <- "log_cnt_rate\\[([0-9]+),([0-9]+),([0-9]+),([0-9]+)\\]"
  dt_posterior$wave <- as.numeric(gsub(pattern, "\\1", dt_posterior$variable))
  dt_posterior$gender_pair_idx <- as.numeric(gsub(pattern, "\\2", dt_posterior$variable))
  dt_posterior$age_idx <- as.numeric(gsub(pattern, "\\3", dt_posterior$variable))
  dt_posterior$alter_age_idx <- as.numeric(gsub(pattern, "\\4", dt_posterior$variable))

  # Recover age and gender
  dt_posterior[, age := age_idx - 1]
  dt_posterior[, alter_age := alter_age_idx - 1]
  dt_posterior[, gender := ifelse(gender_pair_idx %in% c(1,3), "Male",
                        ifelse(gender_pair_idx %in% c(2,4), "Female", NA))]
  dt_posterior[, alter_gender := ifelse(gender_pair_idx %in% c(1,4), "Male",
                              ifelse(gender_pair_idx %in% c(2,3), "Female", NA))]

  # Remove unnecessary columns
  dt_posterior[, c('age_idx', 'alter_age_idx', 'gender_pair_idx') := NULL]

  # Merge with population data
  dt_posterior <- merge(dt_posterior, dt_population,
                        by.x = c("alter_age", "alter_gender"),
                        by.y = c("age", "gender"),
                        all.x = TRUE)

  # Calculate posterior contact intensities
  dt_posterior[, value := exp(value + log(pop))]

  print( head(dt_posterior) ) # DEBUGGING

  return(dt_posterior)
}
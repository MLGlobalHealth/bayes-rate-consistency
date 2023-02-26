#' Extract posterior draws for log_cnt_rates
#'
#' This function takes in posterior draws and extracts the samples for the
#' logarithm of the contact rates by gender and age strata. It returns a data.table
#' with columns for gender_pair_idx, age_idx, alter_age_idx, and value.
#'
#' @param posterior_draws A list of posterior draws obtained from Stan.
#'
#' @return A data.table containing the posterior samples for log(contact rates)
#'
#' @import data.table
#' @importFrom reshape2 melt
#' @importFrom stringr str_match
#'
#' @export
posterior_log_contact_rates <- function(posterior_draws){
  posterior_draws <- subset(posterior_draws, "log_cnt_rate")
  dt_posterior <- as.data.table(reshape2::melt(posterior_draws))

  # Extract indices
  pattern <- "log_cnt_rate\\[([0-9]+),([0-9]+),([0-9]+)\\]"
  indices <- str_match(dt_posterior$variable, pattern)[,2:4]
  dt_posterior$gender_pair_idx <- as.numeric(indices[,1])
  dt_posterior$age_idx <- as.numeric(indices[,2])
  dt_posterior$alter_age_idx <- as.numeric(indices[,3])

  return(dt_posterior)
}

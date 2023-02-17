library(data.table)
library(reshape2)
library(stringr)

#' Perform posterior predictive check
#'
#' This function performs posterior predictive check to assess model fit
#' by comparing observed data with simulated data from the posterior predictive
#' distribution. It returns a data.table containing quantiles of the posterior
#' predictive distribution, and calculates the proportion of observed data
#' within the posterior predictive 95% credible interval (CI).
#'
#' @param fit A model fit object that has the `draws()` method to obtain posterior samples
#' @param dt A data.table or data.frame containing observed data
#' @param ps A vector of probabilities for calculating quantiles of the posterior
#' predictive distribution. Default is c(0.5, 0.025, 0.975)
#' @param p_labs A vector of labels for the quantiles. Default is c('M', 'CL', 'CU')
#' @param outdir A character string specifying the directory to save the results.
#' If not specified, the results are not saved. Default is NA.
#'
#' @return A data.table containing quantiles of the posterior predictive distribution
#' and proportion of observed data within the posterior predictive 95% CI.
#' @export
sim_posterior_predictive_check <- function(fit, dt,
                                           ps = c(0.5, 0.025, 0.975),
                                           p_labs = c('M','CL','CU'),
                                           outdir = NA){

  # Get posterior samples
  po <- fit$draws("yhat_strata", format="draws_matrix")
  dt_po <- as.data.table(reshape2::melt(po))

  # Extract indicies
  pattern <- "yhat_strata\\[([0-9]+),([0-9]+),([0-9]+)\\]"
  indices <- str_match(dt_po$variable, pattern)[,2:4]
  dt_po$gender_pair_idx <- as.numeric(indices[,1])
  dt_po$age_idx <- as.numeric(indices[,2])
  dt_po$alter_age_strata_idx <- as.numeric(indices[,3])

  # Calculate quantiles
  dt_po <- dt_po[, .(q = quantile(value, prob=ps, na.rm=T), q_label = p_labs),
                 by = .(gender_pair_idx, age_idx, alter_age_strata_idx)]
  dt_po <- data.table::dcast(dt_po, gender_pair_idx + age_idx + alter_age_strata_idx ~ q_label, value.var = "q")

  # Recover gender and alter gender
  dt_po[, gender := fcase(
    gender_pair_idx %in% c(1,3), "Male",
    gender_pair_idx %in% c(2,4), "Female",
    default = NA)]

  dt_po[, alter_gender := fcase(
    gender_pair_idx %in% c(1,4), "Male",
    gender_pair_idx %in% c(2,3), "Female",
    default = NA)]

  # Recover age
  dt_po[, age := unique(dt$age)[age_idx]]
  dt_po[, alter_age_strata := sort(unique(dt$alter_age_strata))[alter_age_strata_idx]]

  dt_strata <- unique(dt[, .(age, alter_age_strata, gender, alter_gender, y_strata)])
  dt_strata <- merge(dt_strata, dt_po[, .(age, alter_age_strata, gender, alter_gender, CL, CU, M)],
                     all.x = TRUE,
                     by=c("age", "alter_age_strata", "gender", "alter_gender"))

  dt_strata[, inside.CI := between(y_strata, CL, CU)]
  prop_inside <- mean(dt_strata$inside.CI, na.rm = TRUE)
  cat(" Proportion of points within posterior predictive 95% CI: ", prop_inside, "\n")

  if(!is.na(outdir)){
    saveRDS(dt_strata, file.path(outdir, "ppc.rds"))
  } else {
    warning("\n outdir is not specified. Results were not saved.")
  }

  return(dt_strata)
}

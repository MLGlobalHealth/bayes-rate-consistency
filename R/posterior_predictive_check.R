posterior_predictive_check <- function(posterior_draws,
                                       dt_contacts,
                                       single_contact_age = FALSE,
                                       outdir = NA){
  ps <- c(0.5, 0.025, 0.975)
  p_labs <- c('M','CL','CU')

  dt_posterior <- as.data.table(reshape2::melt(posterior_draws))

  # Extract indicies
  pattern <- "yhat_strata\\[([0-9]+),([0-9]+),([0-9]+)\\]"
  indices <- str_match(dt_posterior$variable, pattern)[,2:4]
  dt_posterior$gender_pair_idx <- as.numeric(indices[,1])
  dt_posterior$age_idx <- as.numeric(indices[,2])
  dt_posterior$alter_age_idx <- as.numeric(indices[,3])

  # Calculate quantiles
  dt_posterior <- dt_posterior[, .(q = quantile(value, prob=ps, na.rm=T), q_label = p_labs),
                               by = .(gender_pair_idx, age_idx, alter_age_idx)]
  dt_posterior <- data.table::dcast(dt_posterior,
                                    gender_pair_idx + age_idx + alter_age_idx ~ q_label,
                                    value.var = "q")

  # Recover age
  dt_posterior[, age := age_idx - 1]

  # Recover gender and alter gender
  dt_posterior[, gender := fcase(
    gender_pair_idx %in% c(1,3), "Male",
    gender_pair_idx %in% c(2,4), "Female",
    default = NA)]

  dt_posterior[, alter_gender := fcase(
    gender_pair_idx %in% c(1,4), "Male",
    gender_pair_idx %in% c(2,3), "Female",
    default = NA)]

  if (!single_contact_age) {
    dt_posterior[, alter_age := alter_age_idx - 1]
    dt_ppc <- merge(dt_contacts, dt_posterior[, .(age, alter_age, gender, alter_gender, CL, CU)],
                    by = c("age", "alter_age", "gender", "alter_gender"))
  } else {
    age_stratum <- sort(unique(dt_contacts$alter_age_strata))
    dt_posterior[, alter_age_strata := age_stratum[alter_age_idx]]
    dt_ppc <- merge(dt_contacts, dt_posterior[, .(age, alter_age_strata, gender, alter_gender, CL, CU)],
                    by = c("age", "alter_age_strata", "gender", "alter_gender"))
  }

  dt_ppc[, inside.CI := between(y, CL, CU)]
  cat(" Proportion of points within posterior predictive 95% CI: ", mean(dt_ppc$inside.CI, na.rm=T), "\n")

  if(!is.na(outdir)){
    saveRDS(dt_ppc, file.path(outdir, "ppc.rds"))
  } else {
    warning("\n outdir is not specified. Results were not saved.")
  }

  return(dt_ppc)
}

#' Calculates posterior contact intensity
#'
#' This function calculates the posterior contact intensity based on the fitted model, using either the full contact intensity matrix or the marginal contact intensity by gender.
#'
#' @param fit A fitted model
#' @param dt_population A data table with columns 'age' and 'pop', representing the age distribution and population size of the study population
#' @param type A character string indicating the type of contact intensity to calculate, either 'matrix' or 'marginal' (default = 'matrix')
#' @param outdir A character string indicating the directory to save the results as an RDS file (default = NA)
#'
#' @return A data table with columns 'age', 'gender' (if type = 'marginal'), 'intensity_M', 'intensity_CL', and 'intensity_CU'
#' @export
#'
#' @examples
#' posterior_contact_intensity(fit, dt_population, type="matrix", outdir="results")
#' posterior_contact_intensity(fit, dt_population, type="marginal", outdir="results")
sim_posterior_contact_intensity <- function(fit, dt_population, type="matrix", outdir=NA){
  dt_posterior <- extract_posterior_rates(fit)

  ps <- c(0.5, 0.025, 0.975)
  p_labs <- c('M','CL','CU')

  if (type == "matrix") { # Full contact intensity matrix
    # Calculate quantiles
    dt_posterior <- dt_posterior[, list(q=quantile(value, prob=ps, na.rm=T), q_label = p_labs),
                   by=list(gender_pair_idx, age_idx, alter_age_idx)]
    dt_posterior <- data.table::dcast(dt_posterior, gender_pair_idx + age_idx + alter_age_idx ~ q_label, value.var = "q")

    # Convert back to rates
    dt_posterior[, M := exp(M)]
    dt_posterior[, CL := exp(CL)]
    dt_posterior[, CU := exp(CU)]

    # Recover age
    dt_posterior[, age := age_idx + 5]
    dt_posterior[, alter_age := alter_age_idx + 5]

    # Recover gender and alter gender
    dt_posterior[, gender := fcase(gender_pair_idx %in% c(1,3), "Male",
                            gender_pair_idx %in% c(2,4), "Female", default = NA)]
    dt_posterior[, alter_gender := fcase(gender_pair_idx %in% c(1,4), "Male",
                                  gender_pair_idx %in% c(2,3), "Female", default = NA)]

    # Load datasets
    dt_posterior <- merge(dt_posterior, dtp, by=c("alter_age", "alter_gender"), all.x = TRUE)

    dt_posterior[, intensity_M := M * pop]
    dt_posterior[, intensity_CL := CL * pop]
    dt_posterior[, intensity_CU := CU * pop]

    if (!is.na(outdir)) {
      saveRDS(dt_posterior, file.path(outdir, "intensity_matrix.rds"))
    } else {
      warning("\n outdir is not specified. Results were not saved.")
    }

    return(dt_posterior)
  } else { # Marginal contact intensity by gender

    # Calculate posterior quantities for Male
    dt_male <- dt_posterior[gender_pair_idx == 1 | gender_pair_idx == 3]
    dt_male[, age := age_idx + 5]
    dt_male[, alter_age := alter_age_idx + 5]

    dt_male <- merge(dt_male,
                     dt_population[gender == "Male"],
                     by = "alter_age",
                     all.x = TRUE)
    dt_male <- dt_male[, .(value = sum(exp(value + log(pop)))), by=c("draw", "age")]
    dt_male <- dt_male[, list( q=quantile(value, prob=ps, na.rm=T), q_label = p_labs), by="age"]
    dt_male[, gender := "Male"]

    # Calculate posterior quantities for Female
    dt_female <- dt_posterior[gender_pair_idx == 2 | gender_pair_idx == 4]
    dt_female[, age := age_idx + 5]
    dt_female[, alter_age := alter_age_idx + 5]

    dt_female <- merge(dt_female,
                       dt_population[gender == "Female"],
                       by = "alter_age",
                       all.x = TRUE)
    dt_female <- dt_female[, .(value = sum(exp(value + log(pop)))), by=c("draw", "age")]
    dt_female <- dt_female[, list( q=quantile(value, prob=ps, na.rm=T), q_label = p_labs), by="age"]
    dt_female[, gender := "Female"]

    # Combine male and female estimates
    dt <- rbind(dt_male, dt_female)
    dt <- data.table::dcast(dt, age + gender ~ q_label, value.var = "q")
    setnames(dt, c("M", "CL", "CU"), c("intensity_M", "intensity_CL", "intensity_CU"))

    if (!is.na(outdir)) {
      saveRDS(dt, file.path(outdir, "intensity_marginal.rds"))
    } else {
      warning("\n outdir is not specified. Results were not saved.")
    }

    return(dt)
  }
}

extract_posterior_rates <- function(fit){
  po <- fit$draws("log_cnt_rate", format="draws_matrix")
  dt_po <- as.data.table(reshape2::melt(po))

  # Extract indices
  pattern <- "log_cnt_rate\\[([0-9]+),([0-9]+),([0-9]+)\\]"

  indices <- str_match(dt_po$variable, pattern)[,2:4]
  dt_po$gender_pair_idx <- as.numeric(indices[,1])
  dt_po$age_idx <- as.numeric(indices[,2])
  dt_po$alter_age_strata_idx <- as.numeric(indices[,3])

  return(dt_po)
}

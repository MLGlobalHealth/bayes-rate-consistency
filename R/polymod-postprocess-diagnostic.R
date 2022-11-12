#' Make convergence diagnostic statistics
#'
#' @param fit A fitted CmdStanModel
#' @param outdir Directory to save outputs
#'
#' @return A summary table of estimates and diagnostics
make_convergence_diagnostic_stats <- function(fit, outdir = NA) {
  fit_summary <- fit$summary()

  # Effective sample size and Rhat
  ess_bulk_sum <- fit_summary$ess_bulk[!is.na(fit_summary$ess_bulk)]
  Rhat_sum <- fit_summary$rhat[!is.na(fit_summary$rhat)]

  cat("\n The minimum and maximum effective sample size are ", range(ess_bulk_sum))
  cat("\n The minimum and maximum Rhat are ", range(Rhat_sum))
  if(min(ess_bulk_sum) < 500) cat('\n Minimum effective sample size smaller than 500')

  # Diagnostics
  sampler_diagnostics <- fit$diagnostic_summary()

  # Compute WAIC and LOO
  tryCatch({
    log_lik <- fit$draws("log_lik", format = "matrix")

    n_inf_log_lik <- sum(is.infinite(log_lik))
    if(n_inf_log_lik > 0){
      .message <- paste("Detected", n_inf_log_lik, "Inf values in log_lik. Removing those iterations.")
      warning(.message)
      log_lik[is.infinite(log_lik)] <- NA
    }
    log_lik <- na.omit(log_lik)
    WAIC <- loo::waic(log_lik)
    LOO <- loo::loo(log_lik)
  }, error = function(e) e)

  # Time of execution
  time <- fit$time()

  # save
  if(!is.na(outdir)){
    saveRDS(ess_bulk_sum, file = file.path(outdir, "ess_bulk_sum.rds"))
    saveRDS(Rhat_sum, file = file.path(outdir, "Rhat_sum.rds"))
    saveRDS(WAIC, file = file.path(outdir, "WAIC.rds"))
    saveRDS(LOO, file = file.path(outdir, "LOO.rds"))
    saveRDS(sampler_diagnostics, file = file.path(outdir, "sampler_diagnostics.rds"))
    saveRDS(time, file = file.path(outdir, "time_elapsed.rds"))
  } else {
    warning("\n outdir is not given. Results were not saved.")
  }

  return(fit_summary)
}

make_ppc <- function(fit, dt, outdir=NA){
  ps <- c(0.5, 0.025, 0.975)
  p_labs <- c('M','CL','CU')

  po <- fit$draws("yhat_strata", format="draws_matrix")
  dt.po <- as.data.table(reshape2::melt(po))

  # Extract indicies
  .pattern <- "yhat_strata\\[([0-9]+),([0-9]+),([0-9]+)\\]"

  dt.po[, comb_idx := as.numeric(gsub(.pattern, "\\1", variable))]
  dt.po[, age_idx := as.numeric(gsub(.pattern, "\\2", variable))]
  dt.po[, alter_age_idx := as.numeric(gsub(.pattern, "\\3", variable))]

  # Calculate quantiles
  dt.po <- dt.po[, list( q=quantile(value, prob=ps, na.rm=T), q_label = p_labs), by=list(comb_idx, age_idx, alter_age_idx)]
  dt.po <- data.table::dcast(dt.po, comb_idx + age_idx + alter_age_idx ~ q_label, value.var = "q")

  # Recover age
  dt.po[, age := age_idx - 1]
  dt.po[, alter_age := alter_age_idx - 1]

  # Recover gender and alter gender
  dt.po[, gender := fcase(
    comb_idx %in% c(1,3), "Male",
    comb_idx %in% c(2,4), "Female",
    default = NA)]

  dt.po[, alter_gender := fcase(
    comb_idx %in% c(1,4), "Male",
    comb_idx %in% c(2,3), "Female",
    default = NA)]

  cat(" Check point 1\n")
  dt.ppc <- merge(dt, dt.po[, list(age, alter_age, gender, alter_gender, CL, CU)],
                  by=c("age", "alter_age", "gender", "alter_gender"))

  cat(" Check point 2\n")
  dt.ppc[, inside.CI := y >= CL & y <= CU]
  cat(" Proportion of points within posterior predictive 95% CI: ", mean(dt.ppc$inside.CI, na.rm=T), "\n")

  if(!is.na(outdir)){
    saveRDS(dt.ppc, file.path(outdir, "ppc.rds"))
  } else {
    warning("\n outdir is not specified. Results were not saved.")
  }

  return(dt)
}

extract_posterior_intensity <- function(fit){
  po <- fit$draws("log_cnt_rate", format="draws_matrix")
  dt.po <- as.data.table(reshape2::melt(po))

  # Extract indices
  .pattern <- "log_cnt_rate\\[([0-9]+),([0-9]+),([0-9]+)\\]"

  dt.po[, comb_idx := as.numeric(gsub(.pattern, "\\1", variable))]
  dt.po[, age_idx := as.numeric(gsub(.pattern, "\\2", variable))]
  dt.po[, alter_age_idx := as.numeric(gsub(.pattern, "\\3", variable))]

  # Recover age and gender
  dt.po[, age := age_idx - 1]
  dt.po[, alter_age := alter_age_idx - 1]
  dt.po[, gender := fcase(comb_idx %in% c(1,3), "Male",
                          comb_idx %in% c(2,4), "Female", default = NA)]
  dt.po[, alter_gender := fcase(comb_idx %in% c(1,4), "Male",
                                comb_idx %in% c(2,3), "Female", default = NA)]

  # Remove unnecessary columns
  dt.po[, age_idx := NULL]
  dt.po[, alter_age_idx := NULL]
  dt.po[, comb_idx := NULL]

  # Merge with population data
  dt.po <- merge(dt.po, dt.pop, by.x = c("alter_age", "alter_gender"), by.y = c("age", "gender"), all.x = TRUE)

  # Calculate posterior contact intensities
  dt.po[, value := exp(value + log(pop))]

  return(dt.po)
}

summarise_posterior_intensity <- function(fit, dt.po, dt.pop, type="matrix", outdir=NA){
  ps <- c(0.5, 0.025, 0.975)
  p_labs <- c('M','CL','CU')

  if(type=="matrix"){ # Full contact intensity matrix
    # Calculate quantiles
    dt <- dt.po[,  .(q=quantile(value, prob=ps, na.rm=T), q_label = p_labs),
                by = .(age, gender, alter_age, alter_gender)]
    dt <- data.table::dcast(dt, age + gender + alter_age + alter_gender ~ q_label, value.var = "q")
    setnames(dt, c("M", "CL", "CU"), c("intensity_M", "intensity_CL", "intensity_CU"))

    if(!is.na(outdir)){
      saveRDS(dt, file.path(outdir, "intensity_matrix.rds"))
    } else {
      warning("\n outdir is not specified. Results were not saved.")
    }

    gc()
    return(dt)

  } else { # Marginal contact intensity by gender

    dt <- dt.po[, .(value = sum(value)), by=c("draw", "age", "gender")]

    # Intensities
    dt.int <- dt[, list( q=quantile(value, probs=ps, na.rm=T), q_label = p_labs), by=c("age", "gender")]
    dt.int <- data.table::dcast(dt.int, age + gender ~ q_label, value.var = "q")
    setnames(dt.int, c("M", "CL", "CU"), c("intensity_M", "intensity_CL", "intensity_CU"))
    setkey(dt.int, age, gender)

    # Ratio between male and females
    dt.m <- dt[gender == "Male"]
    dt.f <- dt[gender == "Female"]

    dt.m[, gender := NULL]
    dt.f[, gender := NULL]
    setnames(dt.f, "value", "valueF")
    setnames(dt.m, "value", "valueM")

    dt.rat <- merge(dt.f, dt.m, by = c("draw", "age"))
    dt.rat[, ratioMF := valueF / valueM]
    dt.rat <- dt.rat[, list( q=quantile(ratioMF, probs=ps, na.rm=T), q_label = p_labs), by="age"]
    dt.rat <- data.table::dcast(dt.rat, age ~ q_label, value.var = "q")
    setnames(dt.rat, c("M", "CL", "CU"), c("ratioMF_M", "ratioMF_CL", "ratioMF_CU"))

    # Combine results into one table
    dt <- merge(dt.int, dt.rat, by = "age", all.x = TRUE, allow.cartesian = TRUE)

    if(!is.na(outdir)){
      saveRDS(dt, file.path(outdir, "intensity_marginal.rds"))
    } else {
      warning("\n outdir is not specified. Results were not saved.")
    }

    return(dt)
  }
}

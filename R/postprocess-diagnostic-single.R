#' Make convergence diagnostic statistics
#'
#' @param fit A fitted CmdStanModel
#' @param outdir Directory to save outputs
#'
#' @return A summary table of estimates and diagnostics
make_convergence_diagnostic_stats <- function(fit, outdir = NA) {

  # Rhat and effective sample size
  fit_summary <- fit$summary(variables = NULL, "rhat", "ess_bulk")

  cat("\n The minimum and maximum effective sample size are ", range(fit_summary$ess_bulk, na.rm = TRUE))
  cat("\n The minimum and maximum Rhat are ", range(fit_summary$rhat, na.rm = TRUE))
  if(min(fit_summary$ess_bulk, na.rm = TRUE) < 500) cat('\n Minimum effective sample size smaller than 500')

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
    saveRDS(fit_summary, file = file.path(outdir, "fit_summary.rds"))
    saveRDS(WAIC, file = file.path(outdir, "WAIC.rds"))
    saveRDS(LOO, file = file.path(outdir, "LOO.rds"))
    saveRDS(sampler_diagnostics, file = file.path(outdir, "sampler_diagnostics.rds"))
    saveRDS(time, file = file.path(outdir, "time_elapsed.rds"))
  } else {
    warning("\n outdir is not given. Results were not saved.")
  }

  return(fit_summary)
}


#' Extract posterior predicted quantities
#'
#' @param fit A fitted CmdStanModel
#' @param dt The dataset used to train the model (data.table)
#' @param predict_type Type of prediction to extract [yhat, yhat_strata]
#'
#' @return A data.table with predictions
#' @export
#'
#' @examples
#' \dontrun{
#'  fit <- readRDS("path/to/model.rds")
#'  dt.po <- extract_posterior_predictions(fit, predict_type = "stratified")
#' }
extract_posterior_predictions <- function(fit, dt){
  ps <- c(0.5, 0.025, 0.975)
  p_labs <- c('M','CL','CU')

  # Predicted contacts
  po <- fit$draws("yhat", inc_warmup = FALSE, format = "draws_matrix")
  dt.po <- as.data.table(reshape2::melt(po))

  # Extract indicies
  .pattern <- "yhat\\[([0-9]+),([0-9]+),([0-9]+)\\]"

  dt.po[, comb_idx := as.numeric(gsub(.pattern, "\\1", variable))]
  dt.po[, age_idx := as.numeric(gsub(.pattern, "\\2", variable))]
  dt.po[, alter_age_idx := as.numeric(gsub(.pattern, "\\3", variable))]

  # Calculate quantiles
  dt.po <- dt.po[, list( q=quantile(value, prob=ps, na.rm=T), q_label = p_labs),
                 by=list(comb_idx, age_idx, alter_age_idx)]
  dt.po <- as.data.table(
    dcast(dt.po, comb_idx + age_idx + alter_age_idx ~ q_label,
          value.var = "q")
  )

  # Recover gender and alter gender
  dt.po[, gender := fcase(
    comb_idx %in% c(1,3), "Male",
    comb_idx %in% c(2,4), "Female",
    default = NA)]

  dt.po[, alter_gender := fcase(
    comb_idx %in% c(1,4), "Male",
    comb_idx %in% c(2,3), "Female",
    default = NA)]

  # Recover age
  dt.po[, age := unique(dt$age)[age_idx]]
  dt.po[, alter_age := unique(dt$alter_age)[alter_age_idx]]
  dt <- merge(dt, dt.po[, list(age, alter_age, gender, alter_gender, CL, CU, M)],
              by=c("age", "alter_age", "gender", "alter_gender"))

  # Calculate contact intensities and rates
  dt[, cntct_intensity_predict := M/part]
  dt[, cntct_intensity_predict_CL := CL/part]
  dt[, cntct_intensity_predict_CU := CU/part]

  dt[, cntct_rate_predict := M/part/pop]
  dt[, cntct_rate_predict_CL := CL/part/pop]
  dt[, cntct_rate_predict_CU := CU/part/pop]

  return(dt)
}

make_ppd_check_covimod <- function(po, dt.survey, outdir=NA){
  ps <- c(0.5, 0.025, 0.975)
  p_labs <- c('M','CL','CU')

  po <- subset(po, "yhat_strata")

  dt.po <- as.data.table(reshape2::melt(po))

  # Extract indicies
  .pattern <- "yhat_strata\\[([0-9]+),([0-9]+),([0-9]+)\\]"

  dt.po[, comb_idx := as.numeric(gsub(.pattern, "\\1", variable))]
  dt.po[, age_idx := as.numeric(gsub(.pattern, "\\2", variable))]
  dt.po[, alter_age_strata_idx := as.numeric(gsub(.pattern, "\\3", variable))]

  # Calculate quantiles
  dt.po <- dt.po[, list( q=quantile(value, prob=ps, na.rm=T), q_label = p_labs),
                 by=list(comb_idx, age_idx, alter_age_strata_idx)]
  dt.po <- data.table::dcast(dt.po, comb_idx + age_idx + alter_age_strata_idx ~ q_label, value.var = "q")

  # Recover gender and alter gender
  dt.po[, gender := fcase(
    comb_idx %in% c(1,3), "Male",
    comb_idx %in% c(2,4), "Female",
    default = NA)]

  dt.po[, alter_gender := fcase(
    comb_idx %in% c(1,4), "Male",
    comb_idx %in% c(2,3), "Female",
    default = NA)]

  # Recover age
  dt.po[, age := unique(dt.survey$age)[age_idx]]
  dt.po[, alter_age_strata := sort(unique(dt.survey$alter_age_strata))[alter_age_strata_idx]]
  dt <- merge(dt.survey, dt.po[, list(age, alter_age_strata, gender, alter_gender, CL, CU, M)],
              by=c("age", "alter_age_strata", "gender", "alter_gender"))

  dt[, inside.CI := y >= CL & y <= CU]
  cat(" Proportion of points within posterior predictive 95% CI: ", mean(dt$inside.CI, na.rm=T), "\n")

  if(!is.na(outdir)){
    saveRDS(dt, file.path(outdir, "ppd_check.rds"))
  } else {
    warning("\n outdir is not specified. Results were not saved.")
  }

  return(dt)
}

extract_posterior_rates <- function(po){
  po <- subset(po, "log_cnt_rate")
  dt.po <- as.data.table(reshape2::melt(po))

  # Extract indices
  .pattern <- "log_cnt_rate\\[([0-9]+),([0-9]+),([0-9]+)\\]"

  dt.po[, comb_idx := as.numeric(gsub(.pattern, "\\1", variable))]
  dt.po[, age_idx := as.numeric(gsub(.pattern, "\\2", variable))]
  dt.po[, alter_age_idx := as.numeric(gsub(.pattern, "\\3", variable))]

  return(dt.po)
}

posterior_contact_intensity <- function(dt.po, dt.pop, type="matrix", simulation=FALSE, outdir=NA){
  ps <- c(0.5, 0.025, 0.975)
  p_labs <- c('M','CL','CU')

  if(type=="matrix"){ # Full contact intensity matrix
    # Calculate quantiles
    dt.po <- dt.po[, list(q=quantile(value, prob=ps, na.rm=T), q_label = p_labs),
                   by=list(comb_idx, age_idx, alter_age_idx)]
    dt.po <- data.table::dcast(dt.po, comb_idx + age_idx + alter_age_idx ~ q_label, value.var = "q")

    # Convert back to rates
    dt.po[, M := exp(M)]
    dt.po[, CL := exp(CL)]
    dt.po[, CU := exp(CU)]

    # Recover age
    if(simulation){ # If simulation data
      dt.po[, age := age_idx + 5]
      dt.po[, alter_age := alter_age_idx + 5]
    } else { # If COVIMOD data
      dt.po[, age := age_idx - 1]
      dt.po[, alter_age := alter_age_idx - 1]
    }

    # Recover gender and alter gender
    dt.po[, gender := fcase(comb_idx %in% c(1,3), "Male",
                            comb_idx %in% c(2,4), "Female", default = NA)]
    dt.po[, alter_gender := fcase(comb_idx %in% c(1,4), "Male",
                                  comb_idx %in% c(2,3), "Female", default = NA)]

    # Load datasets
    dtp <- copy(dt.pop)
    setnames(dtp, c("age", "gender"), c("alter_age", "alter_gender"))
    dt.po <- merge(dt.po, dtp, by=c("alter_age", "alter_gender"), all.x = TRUE)

    dt.po[, intensity_M := M * pop]
    dt.po[, intensity_CL := CL * pop]
    dt.po[, intensity_CU := CU * pop]

    if(!is.na(outdir)){
      saveRDS(dt.po, file.path(outdir, "intensity_matrix.rds"))
    } else {
      warning("\n outdir is not specified. Results were not saved.")
    }

    return(dt.po)
  } else { # Marginal contact intensity by gender

    dtm <- dt.po[comb_idx == 1 | comb_idx == 3]
    if (simulation){
      dtm[, age := age_idx + 5]
    } else {
      dtm[, age := age_idx - 1]
    }

    dtm <- merge(dtm, dt.pop[gender == "Male"], by="age")
    dtm <- dtm[, .(value = sum(exp(value + log(pop)))), by=c("draw", "age")]
    dtm <- dtm[, list( q=quantile(value, prob=ps, na.rm=T), q_label = p_labs), by="age"]
    dtm[, gender := "Male"]
    gc()

    dtf <- dt.po[comb_idx == 2 | comb_idx == 4]
    if (simulation){
      dtf[, age := age_idx + 5]
    } else {
      dtf[, age := age_idx - 1]
    }

    dtf <- merge(dtf, dt.pop[gender == "Female"], by="age")
    dtf <- dtf[, .(value = sum(exp(value + log(pop)))), by=c("draw", "age")]
    dtf <- dtf[, list( q=quantile(value, prob=ps, na.rm=T), q_label = p_labs), by="age"]
    dtf[, gender := "Female"]
    gc()

    dt <- rbind(dtm, dtf)
    dt <- data.table::dcast(dt, age + gender ~ q_label, value.var = "q")
    setnames(dt, c("M", "CL", "CU"), c("intensity_M", "intensity_CL", "intensity_CU"))

    if(!is.na(outdir)){
      saveRDS(dt, file.path(outdir, "intensity_marginal.rds"))
    } else {
      warning("\n outdir is not specified. Results were not saved.")
    }

    return(dt)
  }
}

#' Makes a table for posterior predictive checks
#'
#' @param dt Output of `extract_posterior_predictions()`
#' @param predict_type Type of prediction to extract [yhat, yhat_strata]
#' @param outdir
#'
#' @return A data.table with indications for whether the predicted estimates lie in the posterior predictive 95% CI
#' @export
#'
#' @examples
#' \dontrun{
#' dt.po <- extract_posterior_predictions(fit, predict_type = "yhat")
#'
#' make_posterior_predictive_check(dt.po, predict_type="yhat")
#' }
make_posterior_predictive_check <- function(dt, outdir=NA){

  dt[, inside.CI := cntct_intensity <= cntct_intensity_predict_CU & cntct_intensity >= cntct_intensity_predict_CL]
  cat("\n Proportion of points within posterior predictive 95% CI: ", mean(dt$inside.CI, na.rm=T))

  if(!is.na(outdir)){
    saveRDS(dt, file.path(outdir, "ppd_check.rds"))
  } else {
    warning("\n outdir is not specified. Results were not saved.")
  }

  return(dt)
}

#' Make MSE summary stats table
#'
#' @param dt Output of `extract_posterior_predictions()`
#'
#' @return data.frame with squared bias and MSE values
#' @export
#'
#' @examples
#' \dontrun{
#' dt.po <- extract_posterior_predictions(fit)
#'
#' make_mse_table(dt.po)
#' }
make_error_table <- function(dt, outdir=NA){
  mse <- function(y, y_pred) mean( (y - y_pred)**2, na.rm=T )
  sbias <- function(y, y_pred) mean( y - y_pred, na.rm=T )^2

  df <- data.frame(
    metric = c("bias", "bias", "mse", "mse"),
    name = c("intensity", "rate", "intensity", "rate"),
    value = c(sbias(dt$cntct_intensity ,dt$cntct_intensity_predict),
              sbias(dt$cntct_rate, dt$cntct_rate_predict),
              mse(dt$cntct_intensity ,dt$cntct_intensity_predict),
              mse(dt$cntct_rate, dt$cntct_rate_predict))
  )

  if(!is.na(outdir)){
    saveRDS(df, file = file.path(outdir, "mse.rds"))
  } else {
    return(df)
  }
}

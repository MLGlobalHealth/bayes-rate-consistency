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

  # Time of execution
  time <- fit$time()

  # save
  if(!is.na(outdir)){
    saveRDS(fit_summary, file = file.path(outdir, "fit_summary.rds"))
    saveRDS(sampler_diagnostics, file = file.path(outdir, "sampler_diagnostics.rds"))
    saveRDS(time, file = file.path(outdir, "time_elapsed.rds"))
  } else {
    warning("\n outdir is not given. Results were not saved.")
  }

  return(fit_summary)
}

make_ppd_check <- function(po, contacts, offsets, outdir=NA){
  ps <- c(0.5, 0.025, 0.975)
  p_labs <- c('M','CL','CU')

  po <- subset(po, "yhat_strata")

  dt.po <- as.data.table(reshape2::melt(po))

  # Extract indicies
  .pattern <- "yhat_strata\\[([0-9]+)\\]"

  dt.po[, idx := as.numeric(gsub(.pattern, "\\1", variable))]

  # Calculate quantiles
  dt.po <- dt.po[, list( q=quantile(value, prob=ps, na.rm=T), q_label = p_labs), by=list(idx)]
  dt.po <- data.table::dcast(dt.po, idx ~ q_label, value.var = "q")

  dt <- contacts[order(u, age, alter_age_strata)]
  dt <- rbind(
    dt[gender == "Male" & alter_gender == "Male"],
    dt[gender == "Female" & alter_gender == "Female"],
    dt[gender == "Male" & alter_gender == "Female"],
    dt[gender == "Female" & alter_gender == "Male"]
  )

  dt <- cbind(dt, dt.po)

  dt[, inside.CI := y >= CL & y <= CU]
  cat(" Proportion of points within posterior predictive 95% CI: ", mean(dt$inside.CI, na.rm=T), "\n")

  if(!is.na(outdir)){
    saveRDS(dt, file.path(outdir, "ppd_check.rds"))
  } else {
    warning("\n outdir is not specified. Results were not saved.")
  }

  return(dt)
}

#' Extract log contact rates from a posterior draws object
#'
#' @param po posterior draws matrix
#'
#' @return posterior draws data.table
extract_posterior_intensity <- function(po, dt.pop){
  po <- subset(po, "log_cnt_rate")
  dt.po <- as.data.table(reshape2::melt(po))

  # Extract indices
  .pattern <- "log_cnt_rate\\[([0-9]+),([0-9]+),([0-9]+),([0-9]+)\\]"

  dt.po[, wave := as.numeric(gsub(.pattern, "\\1", variable))]
  dt.po[, comb_idx := as.numeric(gsub(.pattern, "\\2", variable))]
  dt.po[, age_idx := as.numeric(gsub(.pattern, "\\3", variable))]
  dt.po[, alter_age_idx := as.numeric(gsub(.pattern, "\\4", variable))]

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

summarise_posterior_intensity <- function(dt.po, dt.off = NULL, type="matrix", outdir=NA){
  ps <- c(0.5, 0.025, 0.975)
  p_labs <- c('M','CL','CU')

  if(type=="matrix"){ # Full contact intensity matrix
    # Calculate quantiles
    dt <- dt.po[,  .(q=quantile(value, prob=ps, na.rm=T), q_label = p_labs),
                by = .(wave, age, gender, alter_age, alter_gender)]
    dt <- data.table::dcast(dt, wave + age + gender + alter_age + alter_gender ~ q_label, value.var = "q")
    setnames(dt, c("M", "CL", "CU"), c("intensity_M", "intensity_CL", "intensity_CU"))

    if(!is.na(outdir)){
      saveRDS(dt, file.path(outdir, "intensity_matrix.rds"))
    } else {
      warning("\n outdir is not specified. Results were not saved.")
    }

    gc()
    return(dt)

  } else if (type=="sliced") { # Sliced contact intensity (gender combined)
    g <- as.data.table(expand.grid(wave = 1:5, age = 0:84, gender = c("Male", "Female")))
    dt.off <- merge(g, dt.off[, .(wave, age, gender, N)], by=c("wave", "age", "gender"), all.x = T)
    dt.off[is.na(N), N := 1]

    dt <- dt.po[, .(value = sum(value)), by = c("draw", "wave", "age", "gender", "alter_age")]
    dt <- merge(dt, dt.off, by = c("wave", "age", "gender"), all.x = TRUE, allow.cartesian = TRUE)
    dt <- dt[, .(value = sum(value * N) / sum(N)), by=c("draw", "wave", "age", "alter_age")]

    # Calculate ratios from wave 1
    dt.w1 <- dt[wave == 1]
    dt.w1[, wave := NULL]
    setnames(dt.w1, "value", "value_w1")
    dt <- merge(dt, dt.w1, by = c("draw", "age", "alter_age"), all.x = TRUE, allow.cartesian = TRUE)
    dt[, ratio := value / value_w1]

    # Intensities
    dt.int <- dt[, list( q=quantile(value, probs=ps, na.rm=T), q_label = p_labs), by=c("wave", "age", "alter_age")]
    dt.int <- data.table::dcast(dt.int, wave + age + alter_age ~ q_label, value.var = "q")
    setnames(dt.int, c("M", "CL", "CU"), c("intensity_M", "intensity_CL", "intensity_CU"))
    setkey(dt.int, wave, age, alter_age)

    # Ratio
    dt.rat <- dt[, list( q=quantile(ratio, probs=ps, na.rm=T), q_label = p_labs), by=c("wave", "age", "alter_age")]
    dt.rat <- data.table::dcast(dt.rat, wave + age + alter_age ~ q_label, value.var = "q")
    setnames(dt.rat, c("M", "CL", "CU"), c("ratio_M", "ratio_CL", "ratio_CU"))
    setkey(dt.rat, wave, age, alter_age)

    dt <- merge(dt.int, dt.rat, by = c("wave", "age", "alter_age"))

    if(!is.na(outdir)){
      saveRDS(dt, file.path(outdir, "intensity_sliced.rds"))
    } else {
      warning("\n outdir is not specified. Results were not saved.")
    }

    gc()
    return(dt)

  } else if (type=="margin-a") { # Marginal contact intensity by gender

    dt <- dt.po[, .(value = sum(value)), by=c("draw", "wave", "age", "gender")]

    # Intensities
    dt.int <- dt[, list( q=quantile(value, probs=ps, na.rm=T), q_label = p_labs), by=c("wave", "age", "gender")]
    dt.int <- data.table::dcast(dt.int, wave + age + gender ~ q_label, value.var = "q")
    setnames(dt.int, c("M", "CL", "CU"), c("intensity_M", "intensity_CL", "intensity_CU"))
    setkey(dt.int, wave, age, gender)

    # Relative change from wave 1
    dt.w1 <- dt[wave == 1]
    dt.w1[, wave := NULL]
    setnames(dt.w1, "value", "value_w1")
    dt <- merge(dt, dt.w1, by = c("draw", "age", "gender"), all.x = TRUE, allow.cartesian = TRUE)
    dt[, relchng := (value - value_w1) / value_w1]

    dt.rel <- dt[, list( q=quantile(relchng, probs=ps, na.rm=T), q_label = p_labs), by=c("wave", "age", "gender")]
    dt.rel <- data.table::dcast(dt.rel, wave + age + gender ~ q_label, value.var = "q")
    setnames(dt.rel, c("M", "CL", "CU"), c("relchng_M", "relchng_CL", "relchng_CU"))
    setkey(dt.rel, wave, age, gender)

    # Ratio between male and females
    dt.m <- dt[gender == "Male"]
    dt.f <- dt[gender == "Female"]

    dt.m[, gender := NULL]
    dt.f[, gender := NULL]
    setnames(dt.f, "value", "valueF")
    setnames(dt.m, "value", "valueM")

    dt.rat <- merge(dt.f, dt.m, by = c("draw", "wave", "age"))
    dt.rat[, ratioMF := valueF / valueM]
    dt.rat <- dt.rat[, list( q=quantile(ratioMF, probs=ps, na.rm=T), q_label = p_labs), by=c("wave", "age")]
    dt.rat <- data.table::dcast(dt.rat, wave + age ~ q_label, value.var = "q")
    setnames(dt.rat, c("M", "CL", "CU"), c("ratioMF_M", "ratioMF_CL", "ratioMF_CU"))

    # Combine results into one table
    dt <- merge(dt.int, dt.rel, by = c("wave", "age", "gender"))
    dt <- merge(dt, dt.rat, by = c("wave", "age"), all.x = TRUE, allow.cartesian = TRUE)

    if(!is.na(outdir)){
      saveRDS(dt, file.path(outdir, "intensity_marginal_a.rds"))
    } else {
      warning("\n outdir is not specified. Results were not saved.")
    }

    return(dt)
  } else if (type=="margin-b") { # Marginal contact intensity by gender pair

    dt <- dt.po[, .(value = sum(value)), by=c("draw", "wave", "age", "gender", "alter_gender")]
    dt <- dt[, list( q=quantile(value, prob=ps, na.rm=T), q_label = p_labs), by=c("wave", "age", "gender", "alter_gender")]
    dt <- data.table::dcast(dt, wave + age + gender + alter_gender ~ q_label, value.var = "q")
    setnames(dt, c("M", "CL", "CU"), c("intensity_M", "intensity_CL", "intensity_CU"))

    if(!is.na(outdir)){
      saveRDS(dt, file.path(outdir, "intensity_marginal_b.rds"))
    } else {
      warning("\n outdir is not specified. Results were not saved.")
    }

    return(dt)

  } else if (type == "margin-c"){ # Categorised by age group

    g <- as.data.table(expand.grid(wave = 1:5, age = 0:84, gender = c("Male", "Female")))
    dt.off <- merge(g, dt.off[, .(wave, age, gender, N)], by=c("wave", "age", "gender"), all.x = T)
    dt.off[is.na(N), N := 1]

    dt <- dt.po[, .(value = sum(value)), by=c("draw", "wave", "gender", "age")]
    # Define participant age groups
    dt[, age_strata := fcase(age < 18, "0-17",
                             age < 30, "18-29",
                             age < 40, "30-39",
                             age < 50, "40-49",
                             age < 60, "50-59",
                             age < 70, "60-69",
                             age >= 70, "70+",
                             default = NA)]
    dt <- merge(dt, dt.off, by = c("wave", "age", "gender"), all.x = TRUE, allow.cartesian = TRUE)
    dt <- dt[, .(value = sum(value * N) / sum(N)), by = c("draw", "wave", "gender", "age_strata")]

    # Calculate relative change from wave 1
    dt.w1 <- dt[wave == 1]
    dt.w1[, wave := NULL]
    setnames(dt.w1, "value", "value_w1")
    dt <- merge(dt, dt.w1, by = c("draw", "gender", "age_strata"), all.x = TRUE, allow.cartesian = TRUE)
    dt[, relchng := (value - value_w1) / value_w1]

    # Intensities
    dt.int <- dt[, .(q = quantile(value, probs=ps, na.rm=T), q_label = p_labs), by=c("wave", "gender", "age_strata")]
    dt.int <- data.table::dcast(dt.int, wave + gender + age_strata ~ q_label, value.var = "q")
    setnames(dt.int, c("M", "CL", "CU"), c("intensity_M", "intensity_CL", "intensity_CU"))

    # Relative change
    dt.rel <- dt[, .(q = quantile(relchng, probs=ps, na.rm=T), q_label = p_labs), by=c("wave", "gender", "age_strata")]
    dt.rel <- data.table::dcast(dt.rel, wave + gender + age_strata ~ q_label, value.var = "q")
    setnames(dt.rel, c("M", "CL", "CU"), c("relchng_M", "relchng_CL", "relchng_CU"))

    dt <- merge(dt.int, dt.rel, by = c("wave", "gender", "age_strata"))

    if(!is.na(outdir)){
      saveRDS(dt, file.path(outdir, "intensity_marginal_c.rds"))
    } else {
      warning("\n outdir is not specified. Results were not saved.")
    }

    return(dt)
  } else {
    return(NA)
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

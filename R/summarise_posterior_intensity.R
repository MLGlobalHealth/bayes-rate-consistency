#' Summarize Posterior Intensity
#'
#' The function calculates different types of intensity measures based on the input data.
#' It uses conditional statements to determine the type of intensity measure to calculate and calls corresponding helper functions.
#' The function also saves the results as RDS files if an output directory is specified.
#'
#' @param dt_posterior A data table containing posterior values.
#' @param dt_off A data table containing offset values (optional).
#' @param type A character string specifying the type of intensity measure to calculate.
#' @param outdir A character string specifying the output directory (optional).
#'
#' @return A data table containing the calculated intensity measures based on the specified type.
#' If `outdir` is specified, the function also saves the result as an RDS file in the specified output directory.
#'
#' @examples
#' dt_posterior <- data.table(...)
#' dt_off <- data.table(...)
#' type <- "matrix"
#' outdir <- "output_directory"
#'
#' result <- summarise_posterior_intensity(dt_posterior, dt_off, type, outdir)
#'
#' @export
summarise_posterior_intensity <- function(dt_posterior, dt_off = NULL, type = "matrix", outdir = NA){
  if (type == "matrix") {
    dt <- summarise_posterior_intensity.matrix(dt_posterior, outdir)
    return(dt)
    
  } else if (type == "sliced") {
    # Calculate sliced intensity
    dt <- summarise_posterior_intensity.sliced(dt_posterior, dt_off, outdir)
    return(dt)

  } else if (type == "margin-a") {
    # Calculate marginal intensity by gender
    dt <- summarise_posterior_intensity.marginal(dt_posterior, outdir)
    return(dt)

  } else if (type == "margin-b") {
    # Calculate marginal intensity by gender pair
    dt <- summarise_posterior_intensity.marginal.pair(dt_posterior, outdir)
    return(dt)

  } else if (type == "margin-c") {
    # Calculate marginal intensity categorized by age group
    dt <- calculate_marginal_intensity_c(dt_posterior, dt_off, outdir)
    return(dt)

  } else {
    return(NA)
  }
}

summarise_posterior_intensity.matrix <- function(dt_posterior, outdir){
  ps <- c(0.5, 0.025, 0.975)
  p_labs <- c('M','CL','CU')

  # Calculate intensity matrix
    dt <- dt_posterior[,  .(q = quantile(value, prob = ps, na.rm = TRUE), q_label = p_labs),
                       by = .(wave, age, gender, alter_age, alter_gender)]
    dt <- data.table::dcast(dt, wave + age + gender + alter_age + alter_gender ~ q_label, value.var = "q")
    setnames(dt, c("M", "CL", "CU"), c("intensity_M", "intensity_CL", "intensity_CU"))

    if(!is.na(outdir)){
      saveRDS(dt, file.path(outdir, "intensity_matrix.rds"))
    } else {
      warning("\n outdir is not specified. Results were not saved.")
    }

    return(dt)
}

summarise_posterior_intensity.sliced <- function(dt_posterior, dt_off, outdir) {
  ps <- c(0.5, 0.025, 0.975)
  p_labs <- c('M','CL','CU')

  g <- as.data.table(expand.grid(wave = 1:5, age = 0:84, gender = c("Male", "Female")))
  dt_off <- g[dt_off[, .(wave, age, gender, N)], on=c("wave", "age", "gender"), allow.cartesian = TRUE]
  dt_off[is.na(N), N := 1]

  dt <- dt_posterior[, .(value = sum(value)), by = .(iteration, wave, age, gender, alter_age)]
  dt <- dt[dt_off, on = c("wave", "age", "gender"), allow.cartesian = TRUE, nomatch = 0L, sum(value * N) / sum(N), by = .EACHI]

  dt.w1 <- dt[wave == 1]
  dt.w1[, wave := NULL]
  setnames(dt.w1, "value", "value_w1")
  dt <- dt[dt.w1, on = c("iteration", "age", "alter_age"), allow.cartesian = TRUE]

  dt[, ratio := value / value_w1]

  dt_intensity <- dt[, .(q = quantile(value, probs = ps, na.rm = TRUE), q_label = p_labs),
                    by = c("wave", "age", "alter_age")]
  dt_intensity <- data.table::dcast(dt_intensity, wave + age + alter_age ~ q_label, value.var = "q")
  setnames(dt_intensity, c("M", "CL", "CU"), c("intensity_M", "intensity_CL", "intensity_CU"))
  setkey(dt_intensity, wave, age, alter_age)

  dt_ratio <- dt[, .(q = quantile(ratio, probs = ps, na.rm = TRUE), q_label = p_labs),
                by = c("wave", "age", "alter_age")]
  dt_ratio <- data.table::dcast(dt_ratio, wave + age + alter_age ~ q_label, value.var = "q")
  setnames(dt_ratio, c("M", "CL", "CU"), c("ratio_M", "ratio_CL", "ratio_CU"))
  setkey(dt_ratio, wave, age, alter_age)

  dt <- merge(dt_intensity, dt_ratio, by = c("wave", "age", "alter_age"))

  if(!is.na(outdir)){
    saveRDS(dt, file.path(outdir, "intensity_sliced.rds"))
  } else {
    warning("\n outdir is not specified. Results were not saved.")
  }
  return(dt)
}

summarise_posterior_intensity.marginal <- function(dt_posterior, outdir) {
  ps <- c(0.5, 0.025, 0.975)
  p_labs <- c('M','CL','CU')

  dt <- dt_posterior[, .(value = sum(value)), by = c("chain", "iteration", "wave", "age", "gender")]

  # Intensities
  dt_intensity <- dt[, list( q = quantile(value, probs = ps, na.rm = TRUE), q_label = p_labs),
                    by = c("wave", "age", "gender")]
  dt_intensity <- data.table::dcast(dt_intensity, wave + age + gender ~ q_label, value.var = "q")
  setnames(dt_intensity, c("M", "CL", "CU"), c("intensity_M", "intensity_CL", "intensity_CU"))
  setkey(dt_intensity, wave, age, gender)

  # Relative change from wave 1
  # dt.w1 <- dt[wave == 1]
  # dt.w1[, wave := NULL]
  # setnames(dt.w1, "value", "value_w1")
  # dt <- merge(dt, dt.w1, by = c("iteration", "age", "gender"), all.x = TRUE, allow.cartesian = TRUE)
  # dt[, relchng := (value - value_w1) / value_w1]

  # dt.rel <- dt[, list( q=quantile(relchng, probs=ps, na.rm=T), q_label = p_labs), by=c("wave", "age", "gender")]
  # dt.rel <- data.table::dcast(dt.rel, wave + age + gender ~ q_label, value.var = "q")
  # setnames(dt.rel, c("M", "CL", "CU"), c("relchng_M", "relchng_CL", "relchng_CU"))
  # setkey(dt.rel, wave, age, gender)

  # Ratio between male and females
  # dt.m <- dt[gender == "Male"]
  # dt.f <- dt[gender == "Female"]

  # dt.m[, gender := NULL]
  # dt.f[, gender := NULL]
  # setnames(dt.f, "value", "valueF")
  # setnames(dt.m, "value", "valueM")

  # dt_ratio <- merge(dt.f, dt.m, by = c("iteration", "wave", "age"))
  # dt_ratio[, ratioMF := valueF / valueM]
  # dt_ratio <- dt_ratio[, list( q=quantile(ratioMF, probs=ps, na.rm=T), q_label = p_labs), by=c("wave", "age")]
  # dt_ratio <- data.table::dcast(dt_ratio, wave + age ~ q_label, value.var = "q")
  # setnames(dt_ratio, c("M", "CL", "CU"), c("ratioMF_M", "ratioMF_CL", "ratioMF_CU"))

  # Combine results into one table
  # dt <- merge(dt_intensity, dt.rel, by = c("wave", "age", "gender"))
  # dt <- merge(dt, dt_ratio, by = c("wave", "age"), all.x = TRUE, allow.cartesian = TRUE)

  if(!is.na(outdir)){
    saveRDS(dt, file.path(outdir, "intensity_marginal.rds"))
  } else {
    warning("\n outdir is not specified. Results were not saved.")
  }

  return(dt)
}

summarise_posterior_intensity.marginal.pair <- function(dt_posterior, outdir) {
  ps <- c(0.5, 0.025, 0.975)
  p_labs <- c('M','CL','CU')

  dt <- dt_posterior[, .(value = sum(value)), by=c("chain", "iteration", "wave", "age", "gender", "alter_gender")]
  dt <- dt[, list( q=quantile(value, prob=ps, na.rm=T), q_label = p_labs), by=c("wave", "age", "gender", "alter_gender")]
  dt <- data.table::dcast(dt, wave + age + gender + alter_gender ~ q_label, value.var = "q")
  setnames(dt, c("M", "CL", "CU"), c("intensity_M", "intensity_CL", "intensity_CU"))

  if(!is.na(outdir)){
    saveRDS(dt, file.path(outdir, "intensity_marginal_pair.rds"))
  } else {
    warning("\n outdir is not specified. Results were not saved.")
  }

  return(dt)
}

# TODO: debug
calculate_marginal_intensity_c <- function(dt_posterior, dt_off, outdir) {
  ps <- c(0.5, 0.025, 0.975)
  p_labs <- c('M','CL','CU')

  g <- as.data.table(expand.grid(wave = 1:5, age = 0:84, gender = c("Male", "Female")))
  dt_off <- merge(g, dt_off[, .(wave, age, gender, N)], by=c("wave", "age", "gender"), all.x = T)
  dt_off[is.na(N), N := 1]

  dt <- dt_posterior[, .(value = sum(value)), by=c("iteration", "wave", "gender", "age")]
  # Define participant age groups
  dt[, age_strata := fcase(age < 18, "0-17",
                           age < 30, "18-29",
                           age < 40, "30-39",
                           age < 50, "40-49",
                           age < 60, "50-59",
                           age < 70, "60-69",
                           age >= 70, "70+",
                           default = NA)]
  dt <- merge(dt, dt_off, by = c("wave", "age", "gender"), all.x = TRUE, allow.cartesian = TRUE)
  dt <- dt[, .(value = sum(value * N) / sum(N)), by = c("iteration", "wave", "gender", "age_strata")]

  # Calculate relative change from wave 1
  dt.w1 <- dt[wave == 1]
  dt.w1[, wave := NULL]
  setnames(dt.w1, "value", "value_w1")
  dt <- merge(dt, dt.w1, by = c("iteration", "gender", "age_strata"), all.x = TRUE, allow.cartesian = TRUE)
  dt[, relchng := (value - value_w1) / value_w1]

  # Intensities
  dt_intensity <- dt[, .(q = quantile(value, probs=ps, na.rm=T), q_label = p_labs), by=c("wave", "gender", "age_strata")]
  dt_intensity <- data.table::dcast(dt_intensity, wave + gender + age_strata ~ q_label, value.var = "q")
  setnames(dt_intensity, c("M", "CL", "CU"), c("intensity_M", "intensity_CL", "intensity_CU"))

  # Relative change
  dt.rel <- dt[, .(q = quantile(relchng, probs=ps, na.rm=T), q_label = p_labs), by=c("wave", "gender", "age_strata")]
  dt.rel <- data.table::dcast(dt.rel, wave + gender + age_strata ~ q_label, value.var = "q")
  setnames(dt.rel, c("M", "CL", "CU"), c("relchng_M", "relchng_CL", "relchng_CU"))

  dt <- merge(dt_intensity, dt.rel, by = c("wave", "gender", "age_strata"))

  if(!is.na(outdir)){
    saveRDS(dt, file.path(outdir, "intensity_marginal_c.rds"))
  } else {
    warning("\n outdir is not specified. Results were not saved.")
  }

  return(dt)
}
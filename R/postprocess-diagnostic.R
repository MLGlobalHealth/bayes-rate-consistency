summarise_posterior_intensity <- function(dt_posterior, dt.off = NULL, type="matrix", outdir=NA){
  ps <- c(0.5, 0.025, 0.975)
  p_labs <- c('M','CL','CU')

  if (type == "matrix") { # Full contact intensity matrix
    # Calculate quantiles
    dt <- dt_posterior[,  .(q=quantile(value, prob=ps, na.rm=T), q_label = p_labs),
                by = .(wave, age, gender, alter_age, alter_gender)]
    dt <- data.table::dcast(dt, wave + age + gender + alter_age + alter_gender ~ q_label, value.var = "q")
    setnames(dt, c("M", "CL", "CU"), c("intensity_M", "intensity_CL", "intensity_CU"))

    if(!is.na(outdir)){
      saveRDS(dt, file.path(outdir, "intensity_matrix.rds"))
    } else {
      warning("\n outdir is not specified. Results were not saved.")
    }

    return(dt)

  } else if (type=="sliced") { # Sliced contact intensity (gender combined)
    g <- as.data.table(expand.grid(wave = 1:5, age = 0:84, gender = c("Male", "Female")))
    dt.off <- merge(g, dt.off[, .(wave, age, gender, N)], by=c("wave", "age", "gender"), all.x = T)
    dt.off[is.na(N), N := 1]

    dt <- dt_posterior[, .(value = sum(value)), by = c("draw", "wave", "age", "gender", "alter_age")]
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

    dt <- dt_posterior[, .(value = sum(value)), by=c("draw", "wave", "age", "gender")]

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

    dt <- dt_posterior[, .(value = sum(value)), by=c("draw", "wave", "age", "gender", "alter_gender")]
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

    dt <- dt_posterior[, .(value = sum(value)), by=c("draw", "wave", "gender", "age")]
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

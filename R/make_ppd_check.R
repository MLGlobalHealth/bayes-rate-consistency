make_ppd_check <- function(po, contacts, offsets, outdir=NA){
  ps <- c(0.5, 0.025, 0.975)
  p_labs <- c('M','CL','CU')

  po <- subset(po, "yhat_strata")

  dt_posterior <- as.data.table(reshape2::melt(po))

  # Extract indicies
  .pattern <- "yhat_strata\\[([0-9]+)\\]"

  dt_posterior[, idx := as.numeric(gsub(.pattern, "\\1", variable))]

  # Calculate quantiles
  dt_posterior <- dt_posterior[, list( q=quantile(value, prob=ps, na.rm=T), q_label = p_labs), by=list(idx)]
  dt_posterior <- data.table::dcast(dt_posterior, idx ~ q_label, value.var = "q")

  dt <- contacts[order(u, age, alter_age_strata)]
  dt <- rbind(
    dt[gender == "Male" & alter_gender == "Male"],
    dt[gender == "Female" & alter_gender == "Female"],
    dt[gender == "Male" & alter_gender == "Female"],
    dt[gender == "Female" & alter_gender == "Male"]
  )

  dt <- cbind(dt, dt_posterior)

  dt[, inside.CI := y >= CL & y <= CU]
  cat(" Proportion of points within posterior predictive 95% CI: ", mean(dt$inside.CI, na.rm=T), "\n")

  if(!is.na(outdir)){
    saveRDS(dt, file.path(outdir, "ppd_check.rds"))
  } else {
    warning("\n outdir is not specified. Results were not saved.")
  }

  return(dt)
}
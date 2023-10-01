#' Summarise the posterior distribution of the reporting fatigue term
#'
#' This function takes a matrix of posterior draws as input and returns a summary of the posterior distribution of the reporting fatigue term. 
#' The summary includes quantiles of the posterior draws, converted to exponential values, and organized in a data.table format.
# 'If the `outdir` parameter is specified, the summary is also saved as an RDS file.
#'
#' @param posterior_draws A matrix of posterior draws.
#' @param outdir (optional) The directory path where the summary should be saved as an RDS file.
#'
#' @return A data.table containing the summary of the posterior distribution of the reporting fatigue term, with columns for each quantile label (`M`, `CL`, `CU`) and rows for each variable.
#'
#' @examples
#' po <- fit$draws(c("log_cnt_rate", "rho"),
#'                inc_warmup = FALSE,
#'                format = "draws_matrix")
#' summarise_posterior_rho(po)
#'
#' @importFrom reshape2 melt
#' @importFrom data.table as.data.table
#' @importFrom data.table dcast
#' @importFrom stats quantile
#' @importFrom base saveRDS
#'
#' @export
summarise_posterior_rho <- function(posterior_draws, outdir = NULL){
  ps <- c(0.5, 0.025, 0.975)
  p_labs <- c('M','CL','CU')

  posterior_draws <- subset(posterior_draws, "rho")
  dt_posterior <- setDT(reshape2::melt(posterior_draws))

  dt_summary <- dt_posterior[, .(q = quantile(value, prob = ps, na.rm = TRUE), q_label = p_labs),
                              by = variable]
  dt_summary[, q := exp(q)]
  dt_summary <- data.table::dcast(dt_summary, variable ~ q_label, value.var = q)

  if(!is.null(outdir)){
    saveRDS(dt_summary, file.path(outdir, "posterior_rho_summary.rds"))
  } else {
    warning("\n outdir is not specified. Results were not saved.")
  }

  return(dt_summary)
}
    
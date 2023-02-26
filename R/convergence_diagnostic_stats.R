library(cmdstanr)
library(loo)

#' Convergence diagnostic statistics
#'
#' This function computes various convergence diagnostic statistics and saves the outputs as RDS files.
#' It also returns a summary table of estimates and diagnostics from a fitted CmdStanModel object.
#'
#' @param fit A fitted CmdStanModel object.
#' @param outdir A character string specifying the directory to save the output RDS files. Default is NA, indicating that the results will not be saved.
#'
#' @details
#' This function calculates the minimum and maximum effective sample size and Rhat, and warns if the minimum effective sample size is smaller than 500.
#' It then computes the diagnostics from the fitted CmdStanModel object, and calculates the widely applicable information criterion (WAIC) and leave-one-out cross-validation (LOO) using the loo package.
#' If there are any infinite values in the log-likelihood, the function removes those iterations and issues a warning.
#' Finally, it saves the diagnostic statistics, WAIC, LOO, and the time of execution as RDS files in the specified directory.
#' If the directory is not given, the function issues a warning that the results were not saved.
#'
#' @return
#' A summary table of estimates and diagnostics from the fitted CmdStanModel object.
#'
#' @examples
#' \dontrun{
#' # Load the CmdStan library
#' library(cmdstanr)
#'
#' # Compute the convergence diagnostic statistics
#' make_convergence_diagnostic_stats(fit, outdir = "results")
#' }
#'
#' @export
convergence_diagnostic_stats <- function(fit, outdir = NA) {
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

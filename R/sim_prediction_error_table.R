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
#' make_error_table(dt, dt.matrix, outdir=export.path)
#' }
sim_prediction_error_table <- function(dt, dt.matrix, outdir=NA){
  sbias <- function(y, y_pred) mean( y - y_pred, na.rm=T )^2
  mse <- function(y, y_pred) mean( (y - y_pred)**2, na.rm=T )
  mae <- function(y, y_pred) mean( abs(y - y_pred), na.rm=T )

  y <- dt[order(age, gender, alter_age, alter_gender)]$cntct_intensity # True intensity
  y_pred <- dt.matrix[order(age, gender, alter_age, alter_gender)]$intensity_M # Estimated intensity

  df <- data.frame(
    metric = c("bias", "mse", "mae"),
    value = c(sbias(y, y_pred), mse(y, y_pred), mae(y, y_pred))
  )

  if(!is.na(outdir)){
    saveRDS(df, file = file.path(outdir, "error.rds"))
  } else {
    return(df)
  }
}

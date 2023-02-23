#' Stratify age into predefined age strata
#'
#' This function takes a data.table object and stratifies the \code{name} column
#' into predefined age strata using the \code{cut} function from the \code{stats}
#' package. The strata are defined as follows:
#'
#' \describe{
#' \item{\code{0-4}}{Ages 0 to 4}
#' \item{\code{5-9}}{Ages 5 to 9}
#' \item{\code{10-14}}{Ages 10 to 14}
#' \item{\code{15-19}}{Ages 15 to 19}
#' \item{\code{20-24}}{Ages 20 to 24}
#' \item{\code{25-34}}{Ages 25 to 34}
#' \item{\code{35-44}}{Ages 35 to 44}
#' \item{\code{45-54}}{Ages 45 to 54}
#' \item{\code{55-64}}{Ages 55 to 64}
#' \item{\code{65-69}}{Ages 65 to 69}
#' \item{\code{70-74}}{Ages 70 to 74}
#' \item{\code{75-79}}{Ages 75 to 79}
#' \item{\code{80-84}}{Ages 80 to 84}
#' \item{\code{85+}}{Ages 85 and over}
#' }
#'
#' @param dt A data.table object.
#' @param name The name of the column containing age values.
#' @param strat_name The desired name for the stratified column.
#' @return A data.table object with the original age column stratified into
#' age strata according to the predefined ranges.
#' @import data.table
#' @importFrom stats cut
#' @examples
#' dt <- data.table(age = c(23, 56, 70, 81))
#' stratify_age(dt)
#'
#' @export
stratify_age <- function(dt, name="age", strat_name="age_strata"){
  dt <- copy(dt)
  dt[, tmp_strata := cut(
    get(name),
    breaks = c(-Inf, 4, 9, 14, 19, 24, 34, 44, 54, 64, 69, 74, 79, 84, Inf),
    labels = c("0-4","5-9","10-14","15-19","20-24","25-34","35-44",
               "45-54","55-64","65-69","70-74","75-79","80-84","85+")
  )]
  setnames(dt, "tmp_strata", strat_name)
  return(dt)
}

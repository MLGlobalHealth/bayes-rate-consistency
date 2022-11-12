#' Stratifies contacts' age into specified age bands
#'
#' @param dt Contact intensity data.table
#' @param strata.scheme Stratification scheme ["3yr", "5yr", "COVIMOD", "CoMix", "None"]
#'
#' @return A data.table with a stratified age column (alter_age_strata)
#' @export
#'
#' @examples
stratify_alter_age <- function(dt, strata.scheme = "5yr"){
  if (strata.scheme == "CoMix") {
    # CoMix age-stratification
    dt[, alter_age_strata := fcase(
      alter_age %in% 6:11,  "6-11",
      alter_age %in% 12:17, "12-17",
      alter_age %in% 18:29, "18-29",
      alter_age %in% 30:39, "30-39",
      alter_age %in% 40:49, "40-49",
      default = NA
    )]

    dt$alter_age_strata <- factor(
      dt$alter_age_strata,
      levels=c("6-11", "12-17", "18-29", "30-39", "40-49")
    )
  } else if (strata.scheme == "COVIMOD") {
    # COVIMOD-like age stratification
    dt[, alter_age_strata := fcase(
      alter_age %in% 6:9,  "6-9",
      alter_age %in% 10:14, "10-14",
      alter_age %in% 15:19, "15-19",
      alter_age %in% 20:24, "20-24",
      alter_age %in% 25:34, "25-34",
      alter_age %in% 35:44, "35-44",
      alter_age %in% 45:49, "45-49",
      default = NA
    )]

    dt$alter_age_strata <- factor(
      dt$alter_age_strata,
      levels=c("6-9", "10-14", "15-19", "20-24", "25-34", "35-44", "45-49")
    )
  } else if (strata.scheme == "3yr") {
    # 3-year age-stratification
    dt[, alter_age_strata := fcase(
      alter_age %in% 6:8,   "6-8",
      alter_age %in% 9:11,  "9-11",
      alter_age %in% 12:14, "12-14",
      alter_age %in% 15:17, "15-17",
      alter_age %in% 18:20, "18-20",
      alter_age %in% 21:23, "21-23",
      alter_age %in% 24:26, "24-26",
      alter_age %in% 27:29, "27-29",
      alter_age %in% 30:32, "30-32",
      alter_age %in% 33:35, "33-35",
      alter_age %in% 36:38, "36-38",
      alter_age %in% 39:41, "39-41",
      alter_age %in% 42:44, "42-44",
      alter_age %in% 45:47, "45-47",
      alter_age %in% 48:49, "48-49",
      default = NA
    )]

    dt$alter_age_strata <- factor(
      dt$alter_age_strata,
      levels=c("6-8", "9-11","12-14","15-17",
               "18-20","21-23","24-26","27-29",
               "30-32","33-35","36-38","39-41",
               "42-44","45-47","48-49")
    )
  } else if (strata.scheme == "5yr") {
    # 5-year age-stratification
    dt[, alter_age_strata := fcase(
      alter_age %in% 6:9,  "6-9",
      alter_age %in% 10:14, "10-14",
      alter_age %in% 15:19, "15-19",
      alter_age %in% 20:24, "20-24",
      alter_age %in% 25:29, "25-29",
      alter_age %in% 30:34, "30-34",
      alter_age %in% 35:39, "35-39",
      alter_age %in% 40:44, "40-44",
      alter_age %in% 45:49, "45-49",
      default = NA
    )]

    dt$alter_age_strata <- factor(
      dt$alter_age_strata,
      levels=c("6-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49")
    )
  } else if (strata.scheme == "5yr-uneven") {
    # Uneven 5yr stratification (mixed with 10 year)
    dt[, alter_age_strata := fcase(
      alter_age %in% 6:9,  "6-9",
      alter_age %in% 10:14, "10-14",
      alter_age %in% 15:24, "15-24",
      alter_age %in% 25:29, "25-29",
      alter_age %in% 30:34, "30-34",
      alter_age %in% 35:44, "35-44",
      alter_age %in% 45:49, "45-49",
      default = NA
    )]

    dt$alter_age_strata <- factor(
      dt$alter_age_strata,
      levels=c("6-9", "10-14", "15-24", "25-29", "30-34", "35-44", "45-49")
    )
  }
  else {
    # No stratification
    dt$alter_age_strata <- dt$alter_age
  }

  return(dt)
}


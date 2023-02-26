#' Generate a data frame with a grid of demographic groups
#'
#' This function generates a data.table with demographic groups that can be used as input
#' for a model. The data frame includes columns for age, age strata, and
#' optionally gender.
#'
#' @param A The maximum age in the model
#' @param U The number of time periods in the model
#' @param gender A logical value indicating whether to include gender in the output
#' @return A data table with demographic groups
#' @examples
#' make_grid(85, 2)
#' make_grid(85, 2, gender = TRUE)
age_gender_grid <- function(A, U = NULL, gender = FALSE) {
  age_strata <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-34", "35-44",
                  "45-54", "55-64", "65-69", "70-74", "75-79", "80-84")

  if (is.null(U)) { # Single wave
    if (gender) {
      g <- expand.grid(age = seq(0, A - 1),
                       alter_age_strata = age_strata,
                       gender = c("Male", "Female"),
                       alter_gender = c("Male", "Female"))
    } else {
      g <- expand.grid(age = seq(0, A - 1),
                       alter_age_strata = age_strata)
    }
  } else { # Multiple waves
    if (gender) {
      g <- expand.grid(u = seq(1, U),
                       age = seq(0, A - 1),
                       alter_age_strata = age_strata,
                       gender = c("Male", "Female"),
                       alter_gender = c("Male", "Female"))
    } else {
      g <- expand.grid(u = seq(1, U),
                       age = seq(0, A - 1),
                       alter_age_strata = age_strata)
    }
  }

  return(data.table(g))
}

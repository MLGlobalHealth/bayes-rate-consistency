#' Calculate contact intensity from a posterior distribution of contact rates
#'
#' This function calculates contact intensity from a posterior distribution of contact rates and population estimates.
#' The output can be either a full contact intensity matrix or a marginal contact intensity by gender.
#'
#' @param dt_posterior data.table containing posterior distribution of contact rates with columns value (contact rate),
#'                     age_idx (age index), alter_age_idx (age index of contact), and gender_pair_idx (gender pair index)
#' @param dt_population data.table containing population counts with columns pop (population count), age (age index), and gender (gender)
#' @param type character string specifying output type. Possible values are "matrix" and "marginal". Default is "matrix".
#' @param outdir character string specifying the directory where the output should be saved. Default is NA (not saved).
#'
#' @return If type = "matrix", returns a data.table with columns age, alter_age, gender, alter_gender, intensity_M, intensity_CL, and intensity_CU,
#'         where intensity_M, intensity_CL, and intensity_CU are the median, lower 95% credible interval, and upper 95% credible interval of the contact intensity,
#'         respectively, and gender and alter_gender are the gender of the focal and contacted individuals, respectively.
#'         If type = "marginal", returns a data.table with columns age, gender, intensity_M, intensity_CL, and intensity_CU, where intensity_M, intensity_CL,
#'         and intensity_CU are the median, lower 95% credible interval, and upper 95% credible interval of the marginal contact intensity by gender.
#'
#' @examples
#' posterior_contact_intensity(dt_posterior, dt_population)
#'
#' @import data.table
#' @importFrom stats quantile
#' @importFrom data.table setnames
#' @importFrom data.table copy
#' @importFrom data.table merge
#' @importFrom data.table dcast
#' @importFrom data.table rbind
#' @importFrom data.table fcase
#' @importFrom utils saveRDS
#'
#' @export
posterior_contact_intensity <- function(dt_posterior,
                                        dt_population,
                                        type = "matrix",
                                        outdir = NA){

  if (type == "matrix") { # Full contact intensity matrix

    dt_posterior <- dt_posterior[, .(M = exp(quantile(value, 0.5, na.rm = TRUE)),
                                     CL = exp(quantile(value, 0.025, na.rm = TRUE)),
                                     CU = exp(quantile(value, 0.975, na.rm = TRUE)),
                                     age = age_idx - 1,
                                     alter_age = alter_age_idx - 1,
                                     gender = fcase(gender_pair_idx %in% c(1,3), "Male",
                                                    gender_pair_idx %in% c(2,4), "Female"),
                                     alter_gender = fcase(gender_pair_idx %in% c(1,4), "Male",
                                                          gender_pair_idx %in% c(2,3), "Female")),
                                 by = .(gender_pair_idx, age_idx, alter_age_idx)]

    dt_posterior <- merge(dt_posterior,
                          dt_population,
                          by.x = c("alter_age", "alter_gender"),
                          by.y = c("age", "gender"),
                          all.x = TRUE)
    dt_posterior[, intensity_M := M * pop]
    dt_posterior[, intensity_CL := CL * pop]
    dt_posterior[, intensity_CU := CU * pop]

    # Subset columns
    dt_posterior <- dt_posterior[, .(age, alter_age, gender, alter_gender,
                                     intensity_M, intensity_CL, intensity_CU)]

    if(!is.na(outdir)){
      saveRDS(dt_posterior, file.path(outdir, "intensity_matrix.rds"))
    } else {
      warning("\n outdir is not specified. Results were not saved.")
    }

    return(dt_posterior)
  } else if (type == "marginal") { # Marginal contact intensity by gender

    summarize_and_quantile <- function(dt) {
      ps <- c(0.5, 0.025, 0.975)
      p_labs <- c('M', 'CL', 'CU')

      dt <- dt[, .(value = sum(exp(value + log(pop)))), by = .(draw, age)]
      dt <- dt[, .(q = quantile(value, prob = ps, na.rm = TRUE), q_label = p_labs), by = age]

      return(dt)
    }

    # Separate male and female data frames
    dt_male <- dt_posterior[gender_pair_idx %in% c(1, 3)]
    dt_female <- dt_posterior[gender_pair_idx %in% c(2, 4)]

    # Recover age
    dt_male[, age := age_idx - 1]
    dt_female[, age := age_idx - 1]

    # Append population counts
    dt_male <- merge(dt_male,
                     dt_population[gender == "Male"],
                     by.x = "alter_age",
                     by.y = "age")
    dt_female <- merge(dt_female,
                       dt_population[gender == "Female"],
                       by.x = "alter_age",
                       by.y = "age")

    # Summarize quantities
    dt_male <- summarize_and_quantile(dt_male)
    dt_female <- summarize_and_quantile(dt_female)

    # Create gender columns
    dt_male[, gender := "Male"]
    dt_female[, gender := "Female"]

    # Combine Male and Female data and relabel columns
    dt <- rbind(dt_male, dt_female)
    dt <- data.table::dcast(dt, age + gender ~ q_label, value.var = "q")
    setnames(dt, c("M", "CL", "CU"), c("intensity_M", "intensity_CL", "intensity_CU"))

    if(!is.na(outdir)){
      saveRDS(dt, file.path(outdir, "intensity_marginal.rds"))
    } else {
      warning("\n outdir is not specified. Results were not saved.")
    }

    return(dt)
  } else {
    stop("Invalid output type")
  }
}

#' Stratifies contacts' age into specified age bands
#'
#' @param datatable Contact intensity data.table
#' @param strata.scheme Stratification scheme ["3yr", "5yr", "5yr-uneven", "COVIMOD", "CoMix"]
#'
#' @return A data.table with a stratified age column (alter_age_strata)
#' @export
#'
#' @examples
stratify_contact_age <- function(datatable, strata.scheme){

  # Define lookup tables
  lookup_table_breaks <- list(
    "CoMix" = c(6, 12, 18, 30, 40, 49),
    "COVIMOD" = c(6, 10, 15, 20, 25, 35, 45, 49),
    "3yr" = c(6, 9, 12, 15, 18, 21, 24, 27, 30, 33, 36, 39, 42, 45, 48, 49),
    "5yr" = c(6, 10, 15, 20, 25, 30, 35, 40, 45, 49),
    "5yr-uneven" = c(6, 10, 15, 25, 30, 35, 45, 49)
  )

  lookup_table_labels <- list(
    "CoMix" = c("6-11", "12-17", "18-29", "30-39", "40-49"),
    "COVIMOD" = c("6-9", "10-14", "15-19", "20-24", "25-34", "35-44", "45-49"),
    "3yr" = c("6-8", "9-11", "12-14", "15-17", "18-20", "21-23", "24-26", "27-29",
              "30-32", "33-35", "36-38", "39-41", "42-44", "45-47", "48-49"),
    "5yr" = c("6-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"),
    "5yr-uneven" = c("6-9", "10-14", "15-24", "25-29", "30-34", "35-44", "45-49")
  )

  # Use lookup table to create alter_age_strata variable
  datatable[, alter_age_strata := cut(alter_age, breaks = lookup_table_breaks[[strata.scheme]],
                               labels = lookup_table_labels[[strata.scheme]], include.lowest = TRUE)]

  # Set factor levels
  datatable[, alter_age_strata := factor(alter_age_strata, levels = lookup_table_labels[[strata.scheme]])]

  return(datatable)
}


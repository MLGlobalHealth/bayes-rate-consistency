#' Load COVIMOD data
#'
#' This function loads the COVIMOD data and the 2011 population data for Germany.
#' The data is stored in an RData file and a CSV file, respectively. The function
#' converts the loaded data to data.tables and preprocesses the age and gender labels.
#'
#' @param path A character string specifying the path to the directory where the data
#'   files are stored.
#'
#' @return A list with four data.tables: `part`, `pop`, `hh`, and `nhh`.
#'   `part` contains participant data, `pop` contains population data,
#'   `hh` contains household data, and `nhh` contains non-household data.
#'
#' @importFrom data.table as.data.table setnames
#' @importFrom utils load read.csv
#'
#' @export
load_covimod_data <- function(path){
  covimod.path <- file.path(path, "data/COVIMOD/COVIMOD_data_2022-10-17.RData")
  load(covimod.path)

  pop.path <- file.path(path, "data/germany-population-2011.csv")
  dt.pop <- as.data.table(read.csv(pop.path))

  # Convert to data.table
  dt.hh <- as.data.table(hh_data)
  dt.nhh <- as.data.table(non_hh_data)
  dt.part <- as.data.table(part_data)

  # Convert from character to binary
  dt.hh[, hh_met_this_day := ifelse(hh_met_this_day == "Yes", 1, 0)]

  # Preprocess age and gender labels
  dt.part <- clean_age_gender_data(dt.part)
  dt.hh <- clean_age_gender_data(dt.hh)
  dt.nhh <- clean_age_gender_data(dt.nhh)

  # Change column names
  setnames(dt.part, c("wave_0", "age_0", "sex", "age_group"), c("wave", "age", "gender", "age_strata"))
  setnames(dt.hh, old = c("age_group", "sex"), new = c("alter_age_strata", "alter_gender"))
  setnames(dt.nhh, old = c("age_group", "sex"), new = c("alter_age_strata", "alter_gender"))

  return( list(part = dt.part, pop = dt.pop, hh = dt.hh, nhh = dt.nhh) )
}

#' Clean and Transform Age and Gender Data
#'
#' This function cleans and transforms a dataset that includes information on
#' age and gender. It replaces inconsistent age group labels with standardized
#' labels and converts age groups and genders to factors with specified levels.
#'
#' @param data A data frame with columns for age group and sex
#'
#' @return A cleaned version of the input data frame with age group and sex
#' converted to factors with specified levels
#'
#' @details The age group labels "<1", "Under 1", and "1-4" are all standardized
#' to "0-4", while "85 years or older" is standardized to "85+". Age groups and
#' genders are converted to factors with the following levels:
#' - age group levels: "0-4","5-9","10-14","15-19","20-24","25-34","35-44",
#' "45-54","55-64","65-69","70-74","75-79","80-84","85+"
#' - sex levels: "Male", "Female"
#'
#' @examples
#' data <- data.frame(age_group = c("<1", "25-34", "85 years or older"),
#'                    sex = c("Male", "Female", "Female"))
#' clean_data <- clean_age_gender_data(data)
#'
#' @import dplyr
#' @importFrom stats factor
#'
#' @export
clean_age_gender_data <- function(data){
  age_group_levels <- c("0-4","5-9","10-14","15-19","20-24","25-34","35-44",
                        "45-54","55-64","65-69","70-74","75-79","80-84","85+")

  data <- data %>%
    mutate(
      age_group = case_when(
        age_group == "<1" | age_group == "Under 1" | age_group == "1-4" ~ "0-4",
        age_group == "85 years or older" ~ "85+",
        TRUE ~ age_group
      ),
      age_group = factor(age_group, levels = age_group_levels),
      sex = factor(sex, levels = c("Male", "Female"))
    )

  return(data)
}




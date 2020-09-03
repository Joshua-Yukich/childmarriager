#' Cohort constructor function (internal)
#'
#' @param start_year year of simulation start.
#' @param end_year year of simulation end
#' @param young_age lowest starting age in simulation in years
#' @param old_age highest starting age in simulation in years


cohort_construct <- function(start_year, end_year, young_age, old_age) {

  max_age <- end_year - start_year + old_age
  length_of_sim <- end_year - start_year
  n_cohorts <- old_age - young_age + 1
  start_ages <- seq(from = young_age, to = old_age, by = 1)

  list("length_of_sim" = length_of_sim, "n_cohorts" = n_cohorts, "start_ages" = start_ages, "max_age" = max_age)
}

#' Cohort constructor function (internal)
#'
#' @param start_year year of simulation start.
#' @param end_year year of simulation end
#' @param young_age lowest starting age in simulation in years
#' @param old_age highest starting age in simulation in years


cohort_construct <- function(start_year, end_year) {


  length_of_sim <- end_year - start_year
  n_cohorts <- end_year - start_year - 1
  start_years <- seq(from = start_year, to = end_year - 1, by = 1)

  list("length_of_sim" = length_of_sim, "n_cohorts" = n_cohorts, "start_years" = start_years, "end_year" = end_year)

}

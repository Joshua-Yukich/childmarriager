#' marriage hazard construct function (internal)
#'
#' @param start_year year of simulation start.
#' @param end_year year of simulation end
#' @param young_age lowest starting age in simulation in years
#' @param old_age highest starting age in simulation in years
#' @param mar_haz data.frame of marriage probaiblities by year (Starting at zero) thru age 24

mar_haz_define <- function(start_year, end_year, young_age, old_age, mar_prob) {

  cohorts <- cohort_construct(start_year = start_year, end_year = end_year, young_age = young_age, old_age = old_age)

    mar_haz_list  <- list()

    for (i in 1:cohorts[["n_cohorts"]]) {

      mar_haz_list[[i]] <- mar_prob[(cohorts[["start_ages"]][i] + 1):cohorts[["max_age"]],"marrige_prob_perc"]

    }

  mar_haz_list
}

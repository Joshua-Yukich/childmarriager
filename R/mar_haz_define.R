#' marriage hazard construct function (internal)
#'
#' @param start_year year of simulation start.
#' @param end_year year of simulation end
#' @param mar_prob list of data.frames of marriage probabilities by year (Starting at zero) thru maximum age
#' one for each cohort

mar_haz_define <- function(start_year, end_year, mar_prob) {

  cohorts <- cohort_construct(start_year = start_year, end_year = end_year)

    mar_haz_list  <- list()

    for (i in 1:cohorts[["n_cohorts"]]) {

      mar_haz_list[[i]] <- mar_prob[[i]][0:(end_year - (cohorts[["start_years"]][i])),"marrige_prob_perc"]

    }

  mar_haz_list
}

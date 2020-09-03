#'model runs function (internal)
#'
#' @param start_year year of simulation start.
#' @param end_year year of simulation end
#' @param young_age lowest starting age in simulation in years
#' @param old_age highest starting age in simulation in years
#' @param state_names a vector of state names
#' @param country three letter abbreviation of country WHO standard
#' @param mar_prob a vector of marriage probabilities (equal in length to ncycles)
#' @param impact a number
#' @param sex sex for life table queries (0 = female) the default otherwise male

model_runs <- function(start_year, end_year, young_age, old_age,
                          country, mar_prob, impact, sex = 0,
                        state_names = c("Unmarried", "Married", "Dead")) {

  cohorts <- cohort_construct(start_year = start_year, end_year = end_year,
                               young_age = young_age, old_age = old_age)

  params <- params_define(start_year = start_year, end_year = end_year, young_age = young_age, old_age = old_age,
                         country = country, mar_prob = mar_prob, impact = impact, sex = sex)

  trans <- trans_define(start_year = start_year, end_year = end_year, young_age = young_age, old_age = old_age,
                         country = country, mar_prob = mar_prob , sex = sex)

  strats <- strats_define(start_year = start_year, end_year = end_year, young_age = young_age, old_age = old_age,
                 country = country, mar_prob = mar_prob , sex = sex)

  res <- list()

  for (i in 1:cohorts[["n_cohorts"]]) {

    res_mod <- run_model(
      standard = strats[[i]]$strat_standard,
      np1      = strats[[i]]$strat_np1,
      parameters = params[[i]],
      init = c(10000, 0, 0),
      cycles = 1,
      cost = cost,
      effect = utility
    )


    res[[i]] <- res_mod
  }

  res

}

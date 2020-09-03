#' strat define function (internal)
#'
#' @param state_names a vector of state names
#' @param country three letter abbreviation of country WHO standard
#' @param mar_prob a vector of marriage probabilities (equal in length to runtime)
#' @param impact a number
#' @parmam sex sex for life table queries (0 = female) the default otherwise male

strats_define <- function(start_year, end_year, young_age, old_age,
                          country, mar_prob, impact = 1.1, sex = 0, state_names = c("Unmarried", "Married", "Dead")) {

  cohorts <- cohort_construct(start_year = start_year, end_year = end_year, young_age = young_age, old_age = old_age)
  trans <- trans_define(start_year = start_year, end_year = end_year, young_age = young_age, old_age = old_age,
                      country = country, mar_prob = mar_prob , sex = sex)
  strats <- list()

  for (i in 1:cohorts[["n_cohorts"]]) {

    strat_standard <- define_strategy(
      transition = trans[[i]]$mat_standard,
      Unmarried = define_state(cost = 0, utility = 0),
      Married = define_state(cost = 0, utility = 0),
      Dead = define_state(cost = 0, utility = 0)

    )


    strat_np1 <- define_strategy(
      transition = trans[[i]]$mat_np1,
      Unmarried = define_state(cost = 0, utility = 0),
      Married = define_state(cost = 0, utility = 0),
      Dead = define_state(cost = 0, utility = 0)

    )


    strats[[i]] <- list(strat_standard = strat_standard, strat_np1 = strat_np1)
  }

  strats

}

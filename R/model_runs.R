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
require(heemod)
  cohorts <- cohort_construct(start_year = start_year, end_year = end_year,
                               young_age = young_age, old_age = old_age)
  mar_haz_list <- mar_haz_define(start_year, end_year, young_age,
                                 old_age, mar_prob)



  res <- list()

  for (i in 1:cohorts[["n_cohorts"]]) {

     mar_prob_int <- mar_haz_list[[i]]

    params <- define_parameters(
      age_init = cohorts[["start_ages"]][i],
      sex = sex,

      # age increases with cycles
      age = age_init + markov_cycle,

      # age-related mortality rate
      sex_cat = ifelse(sex == 0, "FMLE", "MLE"),
      m_r = get_who_mr(age, sex_cat, country = country, local = FALSE),

      # age-related marriage Probability
      mar_prob_int = mar_prob_int,

      # Marriage Shock Probability
      mar_shock = mar_prob_int * impact
    )



    mat_standard <- define_transition(
      state_names = state_names,
      1 - mar_prob_int - m_r, mar_prob_int, m_r,
      0, 1 - m_r, m_r,
      0, 0, 1
    )


    mat_np1 <- define_transition(
      state_names = state_names,
      1 - mar_shock - m_r, mar_shock, m_r,
      0, 1 - m_r, m_r,
      0, 0, 1
    )

    strat_standard <- define_strategy(
      transition = mat_standard,
      Unmarried = define_state(cost = 0, utility = 0),
      Married = define_state(cost = 0, utility = 0),
      Dead = define_state(cost = 0, utility = 0)

    )


    strat_np1 <- define_strategy(
      transition = mat_np1,
      Unmarried = define_state(cost = 0, utility = 0),
      Married = define_state(cost = 0, utility = 0),
      Dead = define_state(cost = 0, utility = 0)

    )

    res_mod <- run_model(
      standard = strat_standard,
      np1      = strat_np1,
      parameters = params,
      init = c(10000, 0, 0),
      cycles = length(mar_haz_list[[i]]),
      cost = cost,
      effect = utility
    )


    res[[i]] <- res_mod
  }

  res[[i + 1]] <- cohorts
  res

}

#' transitions define function (internal)
#'
#' @param state_names a vector of state names
#' @param country three letter abbreviation of country WHO standard
#' @param runtime a number (to represent the number of annual cycles of model)
#' @param mar_prob a vector of marriage probabilities (equal in length to runtime)
#' @param impact a number
#' @parmam sex sex for life table queries (0 = female) the default otherwise male

trans_define <- function(start_year, end_year, young_age, old_age,
                          country, mar_prob, impact = 1.1, sex = 0, state_names = c("Unmarried", "Married", "Dead")) {

  cohorts <- cohort_construct(start_year = start_year, end_year = end_year, young_age = young_age, old_age = old_age)

  trans <- list()

  for (i in 1:cohorts[["n_cohorts"]]) {

     mat_standard <- define_transition(
      state_names = state_names,
      1 - mar_prob - m_r, mar_prob, m_r,
      0, 1 - m_r, m_r,
      0, 0, 1
    )


    mat_np1 <- define_transition(
      state_names = state_names,
      1 - mar_shock - m_r, mar_shock, m_r,
      0, 1 - m_r, m_r,
      0, 0, 1
    )

   trans[[i]] <- list(mat_standard = mat_standard, mat_np1 = mat_np1)
  }

 trans

}

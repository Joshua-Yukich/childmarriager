#' Parameters define function (internal)
#'
#' @param age_init a numbers.
#' @param country three letter abbreviation of country WHO standard
#' @param runtime a number (to represent the number of annual cycles of model)
#' @param mar_prob a vector of marriage probabilities (equal in length to runtime)
#' @param impact a number
#' @parmam sex sex for life table queries (0 = female) the default otherwise male

params_define <- function(start_year, end_year, young_age, old_age, country, mar_prob, impact, sex = 0) {

cohorts <- cohort_construct(start_year = start_year, end_year = end_year, young_age = young_age, old_age = old_age)

params <- list()

for (i in 1:cohorts[["n_cohorts"]]) {

mar_prob <- mar_prob$marrige_prob_perc[(cohorts[["start_ages"]][i]+1):cohorts[["max_age"]]]

params[[i]] <- define_parameters(
age_init = cohorts[["start_ages"]][i],
sex = sex,

# age increases with cycles
age = age_init + markov_cycle,

# age-related mortality rate
sex_cat = ifelse(sex == 0, "FMLE", "MLE"),
m_r = get_who_mr(age, sex_cat, country = country, local = FALSE),

# age-related marriage Probability
mar_prob = mar_prob,

# Marriage Shock Probability
mar_shock = mar_prob * impact
)
}

params

}



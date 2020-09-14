#' summarize adn weight data function (internal)
#'
#' @param x a list of list of count outcomes from the cout_extract function
#' @param analytic_window a vector containing the start and end year to count marriages


summ_model <- function(x, type = c("cum_inc", "prevalence")) {
  require(dplyr)


  summ_x_1 <- x %>% group_by(year, age, cohort) %>%
    summarize(pop = sum(Unmarried, Married), prop_married = Married/(sum(Unmarried, Married)),
             married = sum(Married))

  if(type == "cum_inc") {
  return(summ_x_1)
  }

  if(type == "prevalence") {
  summ_x_2 <- x %>% filter(age >=20 & age <= 24) %>%
    group_by(year) %>% summarize(pre_18 = sum(Married)/(sum(Unmarried) + sum(Married)))

  return(summ_x_2)
  } else {
    return(x)
  }

}


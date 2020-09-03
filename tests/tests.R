
library(childmarriager)
prob_mar_comp <- data(emp_haz_mal)


cohorts <- cohort_construct(start_year = 2013, end_year = 2014, young_age = 0, old_age = 0)

cohorts[["length_of_sim"]]

runs <- params_define(start_year = 2013, end_year = 2014, young_age = 0, old_age = 0,
               country = "MWI", mar_prob = prob_mar_comp , impact, sex = 0)

trans <- trans_define(start_year = 2013, end_year = 2014, young_age = 0, old_age = 0,
              country = "MWI", mar_prob = prob_mar_comp , sex = 0)

strats <- strats_define(start_year = 2013, end_year = 2014, young_age = 0, old_age = 0,
              country = "MWI", mar_prob = prob_mar_comp , sex = 0)

res <- model_runs(start_year = 2013, end_year = 2033, young_age = 0, old_age = 2,
            country = "MWI", mar_prob = prob_mar_comp, impact = 1.1, sex = 0)

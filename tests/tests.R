
library(childmarriager)
data("emp_haz_mal")
prob_mar_comp <- prob_mar
pad <- data.frame(child_age_years = seq(0,9, by =1), marrige_prob_perc = rep(0, times = 10))
prob_mar_comp <- rbind(pad, prob_mar_comp)
prob_mar_comp[prob_mar_comp$child_age_years >= 18, "marrige_prob_perc"] <- 0

cohorts <- cohort_construct(start_year = 2013, end_year = 2019, young_age = 8, old_age = 12)

cohorts[["length_of_sim"]]
cohorts[["start_ages"]][1]

mar_haz_define(start_year = 2013, end_year = 2019, young_age = 8, old_age = 12, mar_prob = prob_mar_comp)


res <- model_runs(start_year = 2018, end_year = 2035, young_age = 8, old_age = 18,
            country = "MWI", mar_prob = prob_mar_comp, impact = 1.1, sex = 0)

count_extract(res, strategy = "standard")


plot(res[[1]])

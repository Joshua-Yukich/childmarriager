
library(childmarriager)
data("emp_haz_mal")
prob_mar_comp <- prob_mar
pad <- data.frame(child_age_years = seq(0,9, by =1), marrige_prob_perc = rep(0, times = 10))
pad_old <- data.frame(child_age_years = seq(41, 105, by = 1), marrige_prob_perc = rep(0, times = 65))
prob_mar_comp <- rbind(pad, prob_mar_comp, pad_old)
prob_mar_comp[prob_mar_comp$child_age_years >= 18, "marrige_prob_perc"] <- 0



cohorts <- cohort_construct(start_year = 1980, end_year = 2035)

mar_haz_cohorts <- list()

for (i in 1:cohorts[["n_cohorts"]]) {
  prob_mar_comp[,"marrige_prob_perc"] <- prob_mar_comp[,"marrige_prob_perc"] * 0.99
  mar_haz_cohorts[[i]] <- prob_mar_comp
}

cohorts[["length_of_sim"]]
cohorts[["start_years"]]

mar_haz_define(start_year = 1980, end_year = 2035, mar_prob = mar_haz_cohorts)


res <- model_runs(start_year = 1980, end_year = 2035,
            country = "MWI", mar_prob = mar_haz_cohorts, impact = 1.1, sex = 0, init_size =10000, pop_growth = 0.02)

count_extract(res, strategy = "standard")

plot(res[[1]])

data_df_list <- count_extract(res, strategy = "standard")
results <- data_annotate(data_df_list, start_year = 1980, end_year = 2035, init_size = 10000, pop_growth = 0.02)
ggplot(data = results[results$age>=20 & results$age<=24,], aes(x = year, y = Married)) + geom_point()
data_df <- summ_model(results, type = "cum_inc")

ggplot(data = data_df, aes(x = year, y = prop_married, group = cohort)) + geom_line()

data_2 <- summ_model(results, type = "prevalence")

ggplot(data = data_2, aes(x = year, y = pre_18)) + geom_line()

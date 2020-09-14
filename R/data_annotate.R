#' output data packaging function (internal)
#'
#' @param start_year year of simulation start.
#' @param end_year year of simulation end
#' @param x a list of list of count outcomes from the cout_extract function


data_annotate <- function(x, start_year, end_year, pop_growth, init_size) {

  cohorts <- cohort_construct(start_year = start_year, end_year = end_year)

  for (i in 1:length(x)) {
  x[[i]] <- rbind(data.frame(Unmarried = init_size*((1+pop_growth)^(i-1)), Married = 0, Dead = 0), x[[i]])
  }

  for (i in 1:length(x)) {
    x[[i]] <- cbind(x[[i]], age = seq(from = 0,
                                                     to = nrow(x[[i]]) - 1, by = 1))
  }
  for (i in 1:length(x)) {
    x[[i]] <- cbind(x[[i]], cohort = rep(i, times = nrow(x[[i]])))
  }

  for (i in 1:length(x)) {
    x[[i]] <- cbind(x[[i]], year = seq(from = cohorts[["start_years"]][i], to = end_year, by = 1))
  }

  data_frame <- x[[1]]

  if (length(x) > 1) {
  for (i in 2:length(x)) {
  data_frame <- rbind(data_frame, x[[i]])
  }
  }
  data_frame
}


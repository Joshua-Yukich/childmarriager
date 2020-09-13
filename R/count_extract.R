#' count extractor function (internal)
#'
#' @param x list of run_model objects produced by model_runs function
#' @param strategy whether the standard or alternative strategy was used

count_extract <- function(x, strategy = c("standard", "np1")) {

  data_list <- list()

  if (strategy == "standard") {

  for (i in 1:(length(x) - 1)) {
  data_list[[i]] <- x[[i]]$eval_strategy_list$standard$counts

  }
  }



  if (strategy == "np1") {


  for (i in 1:(length(x) - 1)) {
    data_list[[i]] <- x[[i]]$eval_strategy_list$np1$counts

  }
  }

  data_list

}

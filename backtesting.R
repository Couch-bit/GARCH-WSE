pof.test <- function(failures, alpha) {
  failure_count = sum(failures)
  n = length(failures)
  failure_proportion = failure_count / n
  statistic = 2*log(
    ((failure_proportion ^ failure_count)
     * (1 - failure_proportion) ^ (n - failure_count)) 
    / (((alpha) ^ failure_count) * (1 - alpha) ^ (n - failure_count))
  )
  statistic_quant = pchisq(statistic, 1)
  return(1 - statistic_quant)
}

tbfi.test <- function(failures, alpha) {
  failure_count = sum(failures)
  failure_indeces = which(failures == TRUE)
  time_between_failures = c(failure_indeces[1], diff(failure_indeces))
  p_hat = 1 / time_between_failures
  statistic = sum(2*log(
    (p_hat * (1 - p_hat) ^ (time_between_failures - 1)) 
    / ((alpha) * (1 - alpha) ^ (time_between_failures - 1))
  ))
  statistic_quant = pchisq(statistic, failure_count)
  return(1 - statistic_quant)
}

#' Estimator, assuming a normal distribution.
#'
#' This estimator is for comparison purposes in the computational exploration of the estimator for the sample median proposed by varameter.
#'
#' In this case we assume a normal distribution... todo complete this sometime soon.
#'
#' @param median sample median
#' @param spread iqr or range value
#' @param n sample size
#' @param spread_type iqr or range, defaults to iqr
#'
#' @family g_* Possible choices of assumed true density: exponential, Pareto,
#' Cauchy, and log-normal.


g_norm <- function(n, median, spread, spread_type = "iqr") {
  # Estimate parameters.

  # Approximate mean parameter.
  mu <- median

  # Quantile is calculated case-wise for spread type.
  if (spread_type == "iqr") {
    p <- 3 / 4
  } else if (spread_type == "range") {
    p <- (n - 1 / 2) / n
  } else {
    # how to handle errors?
  }

  # Approximate standard deviation parameter.
  sigma <- spread / (2 * qnorm(p))

  # Approximate the standard error of the sample median.
  1 / (2 * sqrt(n) * dnorm(x = median,  mean = mu, sd = sigma))

}

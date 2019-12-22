#' Estimator assuming a Cauchy distribution.
#'
#' This estimator is for comparison purposes in the computational exploration of the estimator for the sample median proposed by varameter.
#'
#' In this case we assume a Cauchy distribution and have an observed median, \eqn{m}, spread (interquartile range or range), \eqn{iqr}, and sample size, \eqn{n}.
#'
#' We have the quantile function given by \eqn{Q(p) = \eta + \gamma \tan(\pi(p - 1/2))}, and the median is equal to the location parameter \eqn{\eta}. To estimate \eqn{\gamma}, we assume \eqn{iqr = Q(3/4) - Q(1/4)}, and solve to show \eqn{\gamma = iqr / 2}.
#'
#' Then, to estimate the error of the sample median, we calculate \eqn{(2 \sqrt{n} g(m; \eta = m, \gamma = iqr / 2) )^{-1}}.
#'
#' @param median sample median
#' @param spread iqr or range value
#' @param n sample size
#' @param spread_type iqr or range, defaults to iqr
#'
#' @family g_* Possible choices of assumed true density: exponential, Pareto,
#' Cauchy, and log-normal.

g_cauchy <- function(n, median, spread, spread_type = "iqr") {
  # Estimate parameters.

  # Location parameter.
  eta <- median

  # Scale parameter.
  if (spread_type == "iqr") {
    theta <- spread / 2
  } else if (spread_type == "range") {
    theta <- (
      spread
    ) / (
      2 * tan(pi * ((n - 0.5) / n ) - 1 / 2)    )
  } else {
  }

  # Approximate the standard error of the sample median.
  1 / (2 * sqrt(n) * dcauchy(x = median, location = eta, scale = theta))

}

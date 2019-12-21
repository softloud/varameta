#' Estimator using the exponential distribution.
#'
#' This estimator is for comparison purposes with the estimator proposed.
#'
#' This estimator assumes we have observed a median, \eqn{m}, and sample size \eqn{n}. We naively assume an exponential distribution \eqn{\sim exp(\lambda)}, so that the median \eqn{\nu := log(2)/\lambda}.
#'
#' This function then estimates \eqn{\lambda  :=  log(2) / m} and approximates the standard error of the sample median \eqn{(2 \sqrt{n} g(m; \lambda = log(2) / m))^{-1}} where \eqn{g} is the exponential density.
#'
#' @param median median
#' @param n sample size
#'
#' @family g_* Possible choices of assumed true density: exponential, Pareto,
#' Cauchy, and log-normal.

g_exp <- function(n, median) {
  # Estimate parameters.
  lambda <- log(2) / median

  # Approximate the standard error of the sample median.
  1 / (2 * sqrt(n) * dexp(median, rate = lambda))

}

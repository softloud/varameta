#' Estimator, assuming a log-normal distribution.
#'
#' This estimator is for comparison purposes in the computational exploration of the estimator for the sample median proposed by varameter. In this case we assume a log-normal distribution
#' \eqn{ \sigma := \frac{1}{\Phi} }
#'
#' @param median sample median
#' @param spread iqr or range value
#' @param n sample size
#' @param spread_type iqr or range, defaults to iqr

g_lnorm <- function(n, median, spread, spread_type = "iqr") {
  # Estimate parameters.

  # Approximate mean parameter.
  mu <- log(median)

  # Quantile is calculated case-wise for spread type.
  if (spread_type == "iqr") {
    p <- 3 / 4
  } else if (spread_type == "range") {
    p <- (n - 1 / 2) / n
  }

  # Approximate standard deviation parameter.
  sigma <- 1 / qnorm(p) *
    log(
      (
        spread * exp(-mu) + sqrt(spread^2 * exp(-2 * mu) + 4)
      ) / 2
    )

  # Approximate the standard error of the sample median.
  1 / (2 * sqrt(n) * dlnorm(x = median,  meanlog = mu, sdlog = sigma))


  }

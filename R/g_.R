#' Estimator using the exponential distribution.
#'
#' These estimators are for comparison purposes with the estimator proposed.
#'
#' This estimator assumes we have observed a m, \eqn{m}, and sample size
#' \eqn{n}. We naively assume an exponential distribution
#' \eqn{\sim exp(\lambda)}, so that the m \eqn{\nu := log(2)/\lambda}.
#'
#' This function then estimates \eqn{\lambda  :=  log(2) / m} and
#' approximates the standard error of the sample
#' \eqn{(2 \sqrt{n} g(m; \lambda = log(2) / m))^{-1}} where \eqn{g}
#' is the exponential density.
#'
#' @param m Sample median.
#' @param n Sample size.
#' @param spread Interquartile range or range value.
#' @param n sample size
#' @param spread_type "iqr" or "range", defaults to "iqr".
#'
#' @family g_* Possible choices of assumed true density: exponential, Pareto,
#' Cauchy, and log-normal.
#' @family one_neet Inputs and outputs neet tested.
#'
#' @name g_
NULL
#> NULL

#' @rdname g_
#' @export

g_exp <- function(n, m) {
  neet::assert_neet(m, "numeric")
  neet::assert_neet(n, "numint")

  # Estimate parameters.
  lambda <- log(2) / m

  # Approximate the standard error of the sample m.
  1 / (2 * sqrt(n) * dexp(m, rate = lambda))

}

#' @rdname g_
#' @export

g_norm <- function(n, m, spread, spread_type = "iqr") {
  neet::assert_neet(n, "numint")
  neet::assert_neet(m, "numeric")
  neet::assert_neet(spread, "numeric")
  neet::assert_neet(spread_type, "character")

  # Estimate parameters.

  # Approximate mean parameter.
  mu <- m

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

  # Approximate the standard error of the sample m.
  1 / (2 * sqrt(n) * dnorm(x = m,  mean = mu, sd = sigma))

}

#' @rdname g_
#' @export

g_lnorm <- function(n, m, spread, spread_type = "iqr") {
  neet::assert_neet(n, "numint")
  neet::assert_neet(m, "numeric")
  neet::assert_neet(spread, "numeric")
  neet::assert_neet(spread_type, "character")

  # Estimate parameters.

  # Approximate mean parameter.
  mu <- log(m)

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

  # Approximate the standard error of the sample m.
  1 / (2 * sqrt(n) * dlnorm(x = m,  meanlog = mu, sdlog = sigma))


}

#' @rdname g_
#' @export

g_cauchy <- function(n, m, spread, spread_type = "iqr") {
  neet::assert_neet(n, "numint")
  neet::assert_neet(m, "numeric")
  neet::assert_neet(spread, "numeric")
  neet::assert_neet(spread_type, "character")

  # Estimate parameters.

  # Location parameter.
  eta <- m

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

  # Approximate the standard error of the sample m.
  1 / (2 * sqrt(n) * dcauchy(x = m, location = eta, scale = theta))

}

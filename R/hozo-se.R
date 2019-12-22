#' Estimate the Stardard Error of the Sample Mean from the Sample Median, Range,
#' and Size
#'
#' See \code{\link{estimator_eqn_ref}} for details and references for equation
#' (6).
#'
#' @param a Minimum value of sample.
#' @param m Median of sample.
#' @param b Maximum value of sample.
#' @param n Sample size.
#'
#' @family mean_error
#'
#' @export

hozo_se <- function(a, m, b, n) {
  s <- if (n <= 15) {
    1 / sqrt(12) * sqrt(
      (b - a)^2 +
        (a - 2 * m + b)^2 / 4
    )
  } else if (n > 15 & n <= 70) {
    (b - a) / 4
  } else if (n > 70) {
    (b - a) / 6
  } else {
    "Error: n should be a sample size."
  }

  return(s / sqrt(n))

}

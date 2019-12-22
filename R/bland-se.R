#' Estimate the Stardard Error of the Sample Mean from Sample Quartiles, Range, and Size
#'
#' See \code{\link{estimator_eqn_ref}} for details and references for equation
#' (11).
#'
#' @param a Minimum value of sample.
#' @param q_1 First quartile of sample.
#' @param m Median of sample.
#' @param q_3 Third quartile of sample.
#' @param b Maximum value of sample.
#' @param n Sample size.
#'
#' @family mean_error
#'
#' @export

bland_se <- function(a, q_1, m, q_3, b, n) {
  sqrt(
    (1 / 16 * (a ^ 2 + 2 * q_1 ^ 2 + 2 * m ^ 2 + 2 * q_3 ^ 2 + b ^ 2) +
      1 / 8 * (a * q_1 + q_1 * m + m * q_3 + q_3 * b) -
      1 / 64 * (a + 2 * q_1 + 2 * m + 2 * q_3 + b) ^ 2
     ) / n)
}

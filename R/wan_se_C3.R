#' Estimate the Stardard Error of the Sample Mean from Sample Quartiles, Range, and Size
#'
#' See \code{\link{estimator_eqn_ref}} for details and references for equation
#' (16).
#'
#' @param q_1 First quartile of sample.
#' @param m Median of sample.
#' @param q_3 Third quartile of sample.
#' @param n Sample size.
#'
#' @family mean_error
#'
#' @export

wan_se_C3 <- function(q_1, m, q_3, n) {
  ((q_3 - q_1) / (2 * qnorm((0.75 * n - 0.125) / (n + 0.25)))) / sqrt(n)
}

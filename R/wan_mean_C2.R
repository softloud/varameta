#' Estimate the Sample Mean from Quartiles, Range, and Size
#'
#' See \code{\link{estimator_eqn_ref}} for details and references for equation
#' (10).
#'
#' @param a Minimum value of sample.
#' @param q_1 First quartile of sample.
#' @param m Median of sample.
#' @param q_3 Third quartile of sample.
#' @param b Maximum value of sample.
#'
#' @export

wan_mean_C2 <- function(a, q_1, m, q_3, b) {
  (a + 2 * q_1 + 2 * m + 2 * q_3 + b) / 8
}

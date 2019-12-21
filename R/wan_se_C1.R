#' Estimate the Standard Error of the Sample Mean from Sample Quartiles and Range
#'
#' See \code{\link{estimator_eqn_ref}} for details and references for equation
#' (9).
#'
#' @param a Minimum value of sample.
#' @param b Maximum value of sample.
#' @param n Sample size.
#'
#' @family mean_error
#'
#' @export

wan_se_C1 <- function(a, b, n) {
  (b - a) / (2 * qnorm((n - 0.375) / (n + 0.25))) / sqrt(n)
}

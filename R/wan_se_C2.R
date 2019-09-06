#' Estimate the Standard Error of the Sample Mean from the Sample Median, Range, and Size
#'
#' See \code{\link{estimator_eqn_ref}} for details and references for equation
#' (13).
#'
#' @param a Minimum value of sample.
#' @param q_1 First quartile of sample.
#' @param m Median of sample.
#' @param q_3 Third quartile of sample.
#' @param b Maximum value of sample.
#' @param n Sample size.
#'
#' @export

wan_se_C2 <- function(a, q_1, m, q_3, b, n) {
    ((b - a) / (4 * qnorm(
      (n - 0.375) / (n + 0.25)
    )) +
    (q_3 - q_1) / (4 * qnorm(
      (0.75 * n - 0.125) / (n + 0.25)
    ))) / sqrt(n)
}

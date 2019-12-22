#' Estimate the Sample Mean from the Sample Median, Range, and Size
#'
#' See \code{\link{estimator_eqn_ref}} for details and references for equation
#' (3).
#'
#' @param a Minimum value of sample.
#' @param m Median of sample.
#' @param b Maximum value of sample.
#'
#' @family mean_estimator
#'
#' @export

hozo_mean <- function(a, m, b) {
  (a + 2 * m + b) / 4
}

#' Estimate the Sample Mean from Sample Median and Sample Range
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

wan_mean_C1 <- function(a, m, b) {
  hozo_mean(a, m, b)
}

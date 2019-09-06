#' Estimate the Sample Mean from the Sample Median and Range
#'
#' See \code{\link{estimator_eqn_ref}} for details and references for equation
#' (14).
#'
#' @param q_1 First quartile of sample.
#' @param m Median of sample.
#' @param q_3 Third quartile of sample.
#'
#' @export

wan_mean_C3 <- function(q_1, m, q_3) {
  (q_1 + m + q_3) / 3
}

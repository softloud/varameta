#' Estimate the Sample Mean from the Sample Median and Range
#'
#' @param q_1 First quartile of sample.
#' @param m Median of sample.
#' @param q_3 Third quartile of sample.
#' @param n Sample size.
#'
#' @family mean_estimator
#'
#' @name wan_c3
NULL
#> NULL

#' @rdname wan_c3
#' @export

wan_mean_C3 <- function(q_1, m, q_3) {
  neet::assert_neet(q_1, "numeric")
  neet::assert_neet(m, "numeric")
  neet::assert_neet(q_3, "numeric")

  (q_1 + m + q_3) / 3
}

#' @rdname wan_c3
#' @export

wan_se_C3 <- function(q_1, m, q_3, n) {
  neet::assert_neet(q_1, "numeric")
  neet::assert_neet(m, "numeric")
  neet::assert_neet(q_3, "numeric")
  neet::assert_neet(n, "numeric")

  ((q_3 - q_1) / (2 * qnorm((0.75 * n - 0.125) / (n + 0.25)))) / sqrt(n)
}

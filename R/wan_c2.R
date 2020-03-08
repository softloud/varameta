#' Estimate the Sample Mean from Quartiles, Range, and Size
#'
#' @param a Minimum value of sample.
#' @param q_1 First quartile of sample.
#' @param m Median of sample.
#' @param q_3 Third quartile of sample.
#' @param b Maximum value of sample.
#' @param n Sample size.
#'
#' @family one_neet Inputs and outputs neet tested.
#' @family meansd_estimator Approximate an estimator for the mean from quartiles.
#'
#' @name wan_c2
NULL
#> NULL

#' @rdname wan_c2
#' @export

wan_mean_C2 <- function(a, q_1, m, q_3, b) {
  neet::assert_neet(a, "numeric")
  neet::assert_neet(q_1, "numeric")
  neet::assert_neet(m, "numeric")
  neet::assert_neet(q_3, "numeric")
  neet::assert_neet(b, "numeric")

  (a + 2 * q_1 + 2 * m + 2 * q_3 + b) / 8
}

#' @rdname wan_c2
#' @export

wan_se_C2 <- function(a, q_1, m, q_3, b, n) {
  neet::assert_neet(a, "numeric")
  neet::assert_neet(q_1, "numeric")
  neet::assert_neet(m, "numeric")
  neet::assert_neet(q_3, "numeric")
  neet::assert_neet(b, "numeric")
  neet::assert_neet(n, "numeric")

    ((b - a) / (4 * qnorm(
    (n - 0.375) / (n + 0.25)
  )) +
    (q_3 - q_1) / (4 * qnorm(
      (0.75 * n - 0.125) / (n + 0.25)
    ))) / sqrt(n)
}

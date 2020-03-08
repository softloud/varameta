#' Estimate Mean from Quartiles and Range (Bland)
#'
#' Method provided by Bland [todo: citation].
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
#' @name bland
NULL
#> NULL

#' @rdname bland
#' @export

bland_mean <- function(a, q_1, m, q_3, b) {
  neet::assert_neet(a, "numeric")
  neet::assert_neet(q_1, "numeric")
  neet::assert_neet(m, "numeric")
  neet::assert_neet(q_3, "numeric")
  neet::assert_neet(b, "numeric")

  (a + 2 * q_1 + 2 * m + 2 * q_3 + b) / 8
}

#' @rdname bland
#' @export

bland_se <- function(a, q_1, m, q_3, b, n) {
  neet::assert_neet(a, "numeric")
  neet::assert_neet(q_1, "numeric")
  neet::assert_neet(m, "numeric")
  neet::assert_neet(q_3, "numeric")
  neet::assert_neet(b, "numeric")
  neet::assert_neet(n, "numeric")

  sqrt(
    (1 / 16 * (a ^ 2 + 2 * q_1 ^ 2 + 2 * m ^ 2 + 2 * q_3 ^ 2 + b ^ 2) +
       1 / 8 * (a * q_1 + q_1 * m + m * q_3 + q_3 * b) -
       1 / 64 * (a + 2 * q_1 + 2 * m + 2 * q_3 + b) ^ 2
    ) / n)
}

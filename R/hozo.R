#' Estimate the Sample Mean from the Sample Median, Range, and Size
#'
#' @param a Minimum value of sample.
#' @param m Median of sample.
#' @param b Maximum value of sample.
#' @param n Sample size.
#'
#' @family one_neet Inputs and outputs neet tested.
#' @family meansd_estimator Approximate an estimator for the mean from quartiles.
#'
#' @name hozo
NULL
#> NULL

#' @rdname hozo
#' @export

hozo_mean <- function(a, m, b) {
  # check inputs are as expected
  neet::assert_neet(a, "numeric")
  neet::assert_neet(m, "numeric")
  neet::assert_neet(b, "numeric")

  (a + 2 * m + b) / 4
}

#' @rdname hozo
#' @export

hozo_se <- function(a, m, b, n) {
  # check inputs are as expected
  neet::assert_neet(a, "numeric")
  neet::assert_neet(m, "numeric")
  neet::assert_neet(b, "numeric")
  neet::assert_neet(n, "integer")

  s <- if (n <= 15) {
    1 / sqrt(12) * sqrt((b - a) ^ 2 +
                          (a - 2 * m + b) ^ 2 / 4)
  } else if (n > 15 & n <= 70) {
    (b - a) / 4
  } else if (n > 70) {
    (b - a) / 6
  }

  return(s / sqrt(n))

}

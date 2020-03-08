#' Estimate the Sample Mean from Sample Median and Sample Range
#'
#' @param a Minimum value of sample.
#' @param m Median of sample.
#' @param b Maximum value of sample.
#' @param n Sample size.
#'
#' @family one_neet Inputs and outputs neet tested.
#' @family meansd_estimator Approximate an estimator for the mean from quartiles.
#'
#' @name wan_c1
NULL
#> NULL

#' @rdname wan_c1
#' @export

wan_mean_C1 <- function(a, m, b) {
  neet::assert_neet(a, "numeric")
  neet::assert_neet(m, "numeric")
  neet::assert_neet(b, "numeric")

    hozo_mean(a, m, b)
}

#' @rdname wan_c1
#' @export

wan_se_C1 <- function(a, b, n) {
  neet::assert_neet(a, "numeric")
  neet::assert_neet(b, "numeric")
  neet::assert_neet(n, "numeric")

    (b - a) / (2 * qnorm((n - 0.375) / (n + 0.25))) / sqrt(n)
}

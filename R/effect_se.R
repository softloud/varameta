#' Calculate the standard deviation of all studies' effects.
#'
#' Estimate the standard error of the effect, depending on how that effect is
#' reported (median or mean).
#'
#' @param centre A sample mean or a median.
#' @param spread The associated measure of spread for the sample mean: either
#' a sample sd, sample interquartile range, or sample range.
#' @param n The sample size.
#' @param centre_type Specify if the center is "mean" or "median".
#' @param spread_type Specify if the spread is reported as "sd", "var", "iqr", or "range".
#'
#' @export

effect_se <- function(centre,
                      spread,
                      n,
                      centre_type = "mean",
                      spread_type = "sd") {
  assertthat::assert_that(
    centre > 0,
    msg = "this function currently requires a positive measure of centre")


  if (centre_type == "mean") {
    if (spread_type == "sd")
      return(spread / sqrt(n))
    else if (spread_type == "var")
      return(sqrt(spread / n))
    else if (spread_type == "se")
      return(spread)
  } else if (centre_type == "median") {
    return(g_lnorm(
      median = centre,
      spread = spread,
      n = n,
      spread_type = spread_type
    ))
  }
}

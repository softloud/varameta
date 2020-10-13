#' Example meta-analysis of medians dataset
#'
#' This example is from TODO put in citation.
#'
#' In this example the sample size `n`, centre `m`, and spread `s`, are presented for the case or treatment group `_t` and the placebo or control group `_c`.
#'
#' @format A tibble.
#' \describe{
#'   \item{study}{Surname of the first author on the paper.}
#'   \item{year}{Year of publication of the paper.}
#'   \item{n_t}{Sample size for the treatment group.}
#'   \item{m_t}{Numerical measure of centre for the treatment group.}
#'   \item{s_t}{List of measures of spread as reported; i.e., ranges and interquartile ranges can be reported as a number or as an interval. The variable `s_t_d` converts this variable to a numeric vector.}
#'   \item{n_c}{Sample size for the control group.}
#'   \item{m_c}{Numerical measure of centre for the control group.}
#'   \item{s_c}{List of measures of spread as reported; i.e., ranges and interquartile ranges can be reported as a number or as an interval. The variable `s_c_d` converts this variable to a numeric vector.}
#'   \item{centre}{Character specifying if centre is a mean or median.}
#'   \item{spread}{Character specifiying if spread is a standard deviation, interquartile range, variance, or range.}
#'   \item{s_t_d}{See `s_t`.}
#'   \item{s_c_d}{See `s_c`.}
#' }

"pinheiro_dat"

#' Reference table for estimators
#'
#' A table of equation references adapted from Table 3 in \href{https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/1471-2288-14-135}{Wan, et al.}'s Estimating the sample mean and standard deviation from the sample size, median, range and/or interquartile range in \emph{BMC Medical Research Methodology}.
#'
#' @format A data frame.
#' \describe{
#'   \item{study}{first name on paper}
#'   \item{mean_eqn}{mean estimator equation identifier in Wan paper}
#'   \item{sd_eqn}{sd estimator equation identifier in Wan paper}
#'   \item{scenario}{set of summary statistics as identified in Wan paper}
#'   \item{summary_stats}{which summary stats are in the scenario, a for minimum, m for median, q_1 for first quartile, q_3 for third quartile, b for max; n for sample size }
#' }
"estimator_ref"

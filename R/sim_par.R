#' Simulation parameters.
#'
#' Combinations of distribtuions and sample sizes to simulate data from. Each row is a different simulation which is replicated over a number of trials.
#'
#' @format A tibble.
#' \describe{
#'   \item{dist}{R-friendly distribution descriptor e.g., \code{exp}, `lnorm`, `weibull`, etc.}
#'   \item{par_1}{First parameter argument for \code{rdist}.}
#'   \item{par_2}{Second parameter argment for \code{rdist}, can be null.}
#'   \item{sample_size}{Sample size.}
#'   \item{rsample}{Sampling function, e.g., \code{rexp}, \code{rlnorm}, \code{rweibull}}
#'   \item{rdensity}{Density function, e.g., \code{dexp}, \code{dlnorm}, \code{dweibull}}
#'   \item{rprob}{Probability function, e.g., \code{pexp}, \code{plnorm}, \code{pweibull}}
#'   \item{sim_id}{Row identifier for this dataframe; so, this variable signifies from which distribution the sampled and for what sample size.}
#'   \item{true_median}{True median of the distribution.}
#'   }
"sim_par"

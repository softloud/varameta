% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/g_.R
\name{g_}
\alias{g_}
\alias{g_exp}
\alias{g_norm}
\alias{g_lnorm}
\alias{g_cauchy}
\title{Estimator using the exponential distribution.}
\usage{
g_exp(n, m)

g_norm(n, m, spread, spread_type = "iqr")

g_lnorm(n, m, spread, spread_type = "iqr")

g_cauchy(n, m, spread, spread_type = "iqr")
}
\arguments{
\item{n}{sample size}

\item{m}{Sample median.}

\item{spread}{Interquartile range or range value.}

\item{spread_type}{"iqr" or "range", defaults to "iqr".}
}
\description{
These estimators are for comparison purposes with the estimator proposed.
}
\details{
This estimator assumes we have observed a m, \eqn{m}, and sample size
\eqn{n}. We naively assume an exponential distribution
\eqn{\sim exp(\lambda)}, so that the m \eqn{\nu := log(2)/\lambda}.

This function then estimates \eqn{\lambda  :=  log(2) / m} and
approximates the standard error of the sample
\eqn{(2 \sqrt{n} g(m; \lambda = log(2) / m))^{-1}} where \eqn{g}
is the exponential density.
}
\seealso{
Other one_neet Inputs and outputs neet tested.: 
\code{\link{bland}},
\code{\link{effect_se}()},
\code{\link{hozo}},
\code{\link{wan_c1}},
\code{\link{wan_c2}}
}
\concept{g_* Possible choices of assumed true density: exponential, Pareto,
Cauchy, and log-normal.}
\concept{one_neet Inputs and outputs neet tested.}

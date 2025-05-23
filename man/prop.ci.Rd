% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prop.ci.R
\name{prop.ci}
\alias{prop.ci}
\title{Confidence interval for proportions}
\usage{
prop.ci(
  count = 522,
  trials = 1000,
  alpha = 0.95,
  nsim = 10000,
  tails = TRUE,
  seed = NULL,
  ci_lim = NULL
)
}
\arguments{
\item{count}{number of successes}

\item{trials}{number of trials}

\item{alpha}{level of confidence}

\item{nsim}{number of simulations}

\item{tails}{proportion for tails (TRUE/FALSE)}

\item{seed}{Set seed to enable identical simulation across calls}

\item{ci_lim}{vector of limits for plotting confidence interval}
}
\value{
standard error of proportion and alpha-level confidence intervals (plots as side effect)
}
\description{
PRodices confidence interval for proportion using approximate process sampling
}
\examples{
prop.ci()

}

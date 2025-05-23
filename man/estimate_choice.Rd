% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate_choice.R
\name{estimate_choice}
\alias{estimate_choice}
\title{Estimate choice exercise}
\usage{
estimate_choice(theta = 0, sd = 1, n_dat = 10, seed = NULL)
}
\arguments{
\item{theta}{mean of normal distribution}

\item{sd}{standard deviation of normal distribution}

\item{n_dat}{sample size}

\item{seed}{seed for random number generator}
}
\value{
pair of estimates
}
\description{
Provides 2 estimates of a population quantity as described in Chapter 5 of Playing With Statistics
}
\examples{
estimate_choice()

}

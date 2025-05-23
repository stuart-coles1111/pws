% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xGfit.R
\name{xGfit}
\alias{xGfit}
\title{Fit an xG model to simulated data}
\usage{
xGfit(xG_data)
}
\arguments{
\item{xG_data}{a list obtained from running xGsim}
}
\value{
List comprising
model: the fitted model. pars: the parameters used in simulating the data.
summary: a comparison of true and estimated parameters
}
\description{
Fits an xG model  to data simulated using the xGsim function
}
\examples{
xG_data <- xGsim()
xGfit(xG_data)
}

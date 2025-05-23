% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xGplot.R
\name{xGplot}
\alias{xGplot}
\title{Plot a fitted xG}
\usage{
xGplot(xG_data, plotlim = 60)
}
\arguments{
\item{xG_data}{a list obtained from running xGsim}

\item{plotlim}{axis limit for plotting}
}
\value{
plots showing fitted xG values and data for simulated xG data
}
\description{
Fits an xG model  to data simulated using the xGsim function
}
\examples{
xG_data <- xGsim(n_data = 10000, seed = 99)
xGplot(xG_data)
}

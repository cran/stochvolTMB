% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/optSV.R
\name{summary.stochvolTMB_predict}
\alias{summary.stochvolTMB_predict}
\title{Calculate quantiles based on predictions from the predictive distribution}
\usage{
\method{summary}{stochvolTMB_predict}(object, ..., quantiles = c(0.025, 0.975), predict_mean = TRUE)
}
\arguments{
\item{object}{A \code{stochvolTMB_summary} object.}

\item{...}{Not used.}

\item{quantiles}{A numeric vector specifying which quantiles to calculate.}

\item{predict_mean}{bool. Should the mean be predicted?}
}
\value{
A list of \code{data.table}s. One for \code{y}, \code{h} and \code{h_exp}.
}
\description{
Calculate quantiles based on predictions from the predictive distribution
}

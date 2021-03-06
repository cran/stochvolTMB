% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/volplot.R
\name{plot.stochvolTMB}
\alias{plot.stochvolTMB}
\title{Plot estimated latent volatility process}
\usage{
\method{plot}{stochvolTMB}(x, ..., include_ci = TRUE, plot_log = TRUE, dates = NULL)
}
\arguments{
\item{x}{A \code{stochvolTMB} object returned form \link{estimate_parameters}.}

\item{...}{Currently not used.}

\item{include_ci}{logical value indicating if volatility should be plotted with approximately 95 \% confidence interval.}

\item{plot_log}{logical value indicating if the estimated should be plotted on log or original scale. 
If \code{plot_log = TRUE} the process h is plotted. 
If \code{plot_log = FALSE} 100 \code{sigma_y} exp(\code{h} / 2) is plotted.}

\item{dates}{vector of length ncol(x$nobs), providing optional dates for labeling the x-axis. 
The default value is NULL; in this case, the axis will be labeled with numbers.}
}
\value{
ggplot object with plot of estimated estimated volatility.
}
\description{
Displays the estimated latent volatility process over time.
}

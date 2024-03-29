% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simSV.R
\name{simulate_parameters}
\alias{simulate_parameters}
\title{Simulate from the asymptotic distribution of the parameter estimates}
\usage{
simulate_parameters(object, nsim = 1000)
}
\arguments{
\item{object}{A \code{stochvolTMB} object.}

\item{nsim}{Number of simulations.}
}
\value{
matrix of simulated values.
}
\description{
Sampling is done on the scale the parameters were estimated. The standard deviations are simulated on log-scale 
and the persistence is simulated on logit scale. The same is true for the correlation parameter in the leverage model.
}

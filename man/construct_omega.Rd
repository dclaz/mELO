% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/construct_omega.R
\name{construct_omega}
\alias{construct_omega}
\title{onstruct Omega matrix for given k}
\usage{
construct_omega(k)
}
\arguments{
\item{k}{Integer defining the complexity of non-transitive interactions to
model.}
}
\value{
a matrix
}
\description{
This function constructs the 2k * 2k Omega matrix necessary for calulating
and updating mELO ratings.
}
\examples{
construct_omega(1)
construct_omega(3)
}

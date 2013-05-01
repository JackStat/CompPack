\name{maxbound}
\alias{maxbound}
\title{Maxbound Optimization}
\usage{
  maxbound(f, lo, hi)
}
\arguments{
  \item{f}{the function to be optimized.}

  \item{lo}{the lower bound.}

  \item{hi}{the upper bound.}
}
\value{
  lo lower bound estimate of the function's optimum.

  hi upper bound estimate of the function's optimum.
}
\description{
  Used to estimate the bounds of a function, in which the
  optimum value will be located within.
}
\examples{
maxbound(sin, 1/2, pi/2)
}
\author{
  Tyler Hunt \email{tyler@psychoanalytix.com}
}


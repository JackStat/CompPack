\name{d1f}
\alias{d1f}
\title{First Derivative}
\usage{
  d1f(f, x)
}
\arguments{
  \item{f}{The function that you want to take the
  derivative of}

  \item{x}{The point or vector of points where the
  derivative will be evaluated}
}
\description{
  The first derivative of a function evaluated at point(s)
  x.

  Differentiation is a method to compute the rate at which
  the dependent output (f(x)) changes with respect to the
  change the independent input (x). The first derrivative
  can be used to obtain an exact value for the slope of the
  tangent line, at the point where the derivative was
  evaluated.
}
\examples{
x = runif(10, pi/3, 3*pi/4)
df(tan, x)
}
\author{
  Damon McCafferty
  \email{damon.mccafferty@economics.utah.edu}
}


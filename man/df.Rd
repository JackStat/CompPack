\name{df}
\alias{df}
\title{Second Derivative}
\usage{
  df(f, x)
}
\arguments{
  \item{f}{The function that you want to take the
  derivative of}

  \item{x}{The point or vector of points where the
  derivative will be evaluated}
}
\description{
  The Second derivative of a function evaluated at point(s)
  x.

  Roughly spoken, the seond derivative is the derivative of
  a derivative. This can be used to measure how the rate of
  change of a quantitty is itself changing.
}
\examples{
x = runif(10, pi/3, 3*pi/4)
df(tan, x)
}
\author{
  Damon McCafferty
  \email{damon.mccafferty@economics.utah.edu}
}


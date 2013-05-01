\name{newton.raph}
\alias{newton.raph}
\title{Newton Raphson Alogorithm}
\usage{
  newton.raph(f, d1f, d2f, start, tol = 1e-06)
}
\arguments{
  \item{f}{The function that you want to optimize.}

  \item{d1f}{the 1st derivative of the function.}

  \item{d2f}{the 2nd derivative of the function.}

  \item{start}{The starting value for optimization.}

  \item{tol}{The level of tolerance desired for
  convergence.}
}
\value{
  Estimate Initial estimate of the optimum value.

  Iterations Number of cycles used to obtain the optimum
  value of the function.

  Liklihood Optimum value
}
\description{
  A method for obtaining successively better approximations
  for the optimal value of a function. Named after Sir
  Isaac Newton and Joseph Raphson.
}
\author{
  Tyler Hunt \email{tyler@psychoanalytix.com}
}


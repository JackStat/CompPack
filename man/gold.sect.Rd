\name{gold.sect}
\alias{gold.sect}
\title{Golden Section Search Optimization}
\usage{
  gold.sect(f, lo, hi, tol = 1e-07)
}
\arguments{
  \item{f}{The function to be optimized}

  \item{lo}{Lower estimate}

  \item{hi}{Upper estimate}

  \item{tol}{Level of tolerance desired for optimization}
}
\value{
  a estimated optimal value of the function

  f(a) function value at the optimal value
}
\description{
  A method used for locating the optimum value of a
  function. The technique utilizes the golden ratio to
  successively narrow the range of values for which the
  optimum is found.
}
\examples{
gold.sect(sin, 1/2, pi/2)
}
\author{
  Tyler Hunt \email{tyler@psychoanalytix.com}
}


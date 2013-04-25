\name{Bootstrap}
\alias{Bootstrap}
\title{Bootstrap}
\usage{
  Bootstrap(x, Boots = 100, fn)
}
\arguments{
  \item{x}{a vector.}

  \item{Boots}{The number of bootstraps.}

  \item{fn}{the function you want to bootstrap, ie., mean,
  var, cov, etc.}
}
\description{
  This function is used for estimating standard errors when
  the distribution is not know.
}
\examples{
x = runif(10, 0, 1)
Bootstrap(x,fn=mean)
}
\author{
  Tyler Hunt \email{tyler@psychoanalytix.com}
}


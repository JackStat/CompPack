\name{bootstrap}
\alias{bootstrap}
\title{bootstrap}
\usage{
  bootstrap(x, boots = 100, fn)
}
\arguments{
  \item{x}{a vector.}

  \item{boots}{The number of bootstraps.}

  \item{fn}{the function you want to bootstrap, ie., mean,
  var, cov, etc.}
}
\description{
  This function is used for estimating standard errors when
  the distribution is not know.
}
\examples{
x = runif(10, 0, 1)
bootstrap(x,fn=mean)
}
\author{
  Tyler Hunt \email{tyler@psychoanalytix.com}
}


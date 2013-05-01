\name{log.lik}
\alias{log.lik}
\title{Find the log likelihood of a normal distribution}
\usage{
  log.lik(x = data, mu, var)
}
\arguments{
  \item{x}{data.}

  \item{mu}{estimated mean.}

  \item{var}{estimated variance.}
}
\value{
  ll logliklihood of the distribution
}
\description{
  Find the log likelihood of a normal distribution.
}
\examples{
x = rnorm(100, 3, 7)
log.lik(x,3,7)
}
\author{
  Damon McCafferty
  \email{damon.mccafferty@economics.utah.edu}
}


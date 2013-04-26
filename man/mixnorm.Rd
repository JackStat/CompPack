\name{mixnorm}
\alias{mixnorm}
\title{Create a Mixture of Normal Distributions}
\usage{
  mixnorm(n, p, mu, sd)
}
\arguments{
  \item{n}{number of observations. If \eqn{length(n) > 1},
  the length is taken to be the number required}

  \item{p}{vector of probabilities}

  \item{mu}{vector of means}

  \item{sd}{vector of standard deviations}
}
\description{
  Create a Mixture of Normal Distributions
}
\examples{
x = mixnorm(100, c(.50, .15, .35), c(-3, 0, 3), c(3,3,3))
hist(x)
}
\author{
  Tyler Hunt \email{tyler@psychoanalytix.com}
}


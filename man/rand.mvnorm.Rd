\name{rand.mvnorm}
\alias{rand.mvnorm}
\title{Generate Multivariate Normal Distribution}
\usage{
  rand.mvnorm(n, mu, sigma)
}
\arguments{
  \item{n}{the number of observations.}

  \item{mu}{a vector of means.}

  \item{sigma}{a variance-covariance matrix.}
}
\description{
  A method for generating a pseudo-random multivariate
  normal data that is based on a cholesky decomposition.
}
\examples{
sigma=matrix(rep(.5, 9), nrow=3)
diag(sigma)<-1
rand.mvnorm(100, c(1,5,11), sigma)
}
\author{
  Tyler Hunt \email{tyler@psychoanalytix.com}
}


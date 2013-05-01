\name{em.mixnorm}
\alias{em.mixnorm}
\title{EM-Algorithm for Normal Distribution}
\usage{
  em.mixnorm(x, k)
}
\arguments{
  \item{x}{the data.}

  \item{k}{estimate of the number of mixtures.}
}
\value{
  itterations number of cycles the program used to
  calculate the estimated means, standard devaitions and
  probabilities.

  means estimated mean of each mixture.

  stddevs estimated standard deviation of each mixture.

  probs estimated probabilities of each mixture.
}
\description{
  The Expectation-Maximization (EM) algorithm is and
  itterative method for locating the maximum estimate for
  the parameters of a mixture of normal distributions. The
  EM iteration alternates between performing an expectation
  (E) step, which creates a function for the expectation of
  the log-likelihood evaluated using the current estimate
  for the parameters, and a maximization (M) step, which
  computes parameters maximizing the expected
  log-likelihood found on the E step. These
  parameter-estimates
}
\examples{
x=mixnorm(100, c(.50, .15, .35), c(-3, 0, 3), c(3,3,3))
em.mixnorm(x,3)
}
\author{
  Tyler Hunt \email{tyler@psychoanalytix.com}
}


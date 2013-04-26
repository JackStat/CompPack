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
  EM-Algorithm for Normal Distribution
}
\examples{
x=mixnorm(100, c(.50, .15, .35), c(-3, 0, 3), c(3,3,3))
em.mixnorm(x,3)
}
\author{
  Tyler Hunt \email{tyler@psychoanalytix.com}
}


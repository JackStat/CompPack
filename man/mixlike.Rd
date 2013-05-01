\name{mixlike}
\alias{mixlike}
\title{mixlike}
\usage{
  mixlike(x, p, m, s)
}
\arguments{
  \item{x}{is the mixture function to be evaluated}

  \item{p}{is the vector of probabilities}

  \item{m}{is the vector if means}

  \item{s}{is the vector of standard deviation}
}
\description{
  A supplement fucntion for uphill.mixnorm, this use to
  compute the log likelihood funciotn for the mixture of
  normal distribution
}
\examples{
mixlike(mixnorm(100, c(.50, .15, .35), c(-3, 0, 3), c(3,3,3)), c(.5,.5), c(-5,5), c(1,1))
}
\author{
  Ernest Chan \email{faiernest418@gmail.com}
}


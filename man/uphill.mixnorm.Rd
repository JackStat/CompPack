\name{uphill.mixnorm}
\alias{uphill.mixnorm}
\title{uphill.mixnorm}
\usage{
  uphill.mixnorm(x, k, n = 1000, t = 100)
}
\arguments{
  \item{x}{is the function of the mixture}

  \item{k}{is the number of mixture}

  \item{n}{is the number of iteration}

  \item{t}{is the temperature}
}
\description{
  This function is use for conducting a uphill search on
  the mixture of normal distribution
}
\examples{
uphill.mixnorm(mixnorm(100, c(.50, .15, .35), c(-3, 0, 3), c(3,3,3)), 4, 1000, 10)
}
\author{
  Ernest Chan \email{faiernest418@gmail.com}
}


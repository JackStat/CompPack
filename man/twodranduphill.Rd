\name{twodranduphill}
\alias{twodranduphill}
\title{uphill search on matrix function}
\usage{
  twodranduphill(f, x, y, s, n = 100, t = 100)
}
\arguments{
  \item{f}{matrix of data set}

  \item{y}{is the starting point for the search}

  \item{x}{is the starting point for the search}

  \item{n}{is the number of iteration for the search}

  \item{s}{standard devation for each search jump, default
  at 1}

  \item{t}{is the tempearture, where its default at 100}
}
\description{
  This function is use to do a random uphill search on any
  2 demision data set
}
\examples{
twodranduphill(twodfunction, 3, 2, 1, 10, 1)
}
\author{
  Ernest Chan \email{faiernest418@gmail.com}
}


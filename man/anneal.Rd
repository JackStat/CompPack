\name{anneal}
\alias{anneal}
\title{anneal}
\usage{
  anneal(f, mu, n = 1000, sig = 1, tt = 10, g = 0.999)
}
\arguments{
  \item{f}{function to find the maximum}

  \item{mu}{starting point}

  \item{n}{number of iterations}

  \item{sig}{size of steps}

  \item{tt}{temperature}

  \item{g}{rate of temperature}
}
\description{
  General anneal function, where we conduct a uphill search
  on the function to find maximum
}
\examples{
#anneal(sin, 0, 100, 1, 10, .99)
}
\author{
  Ernest Chan \email{faiernest418@gmail.com}
}


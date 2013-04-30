\name{pois.rate}
\alias{pois.rate}
\title{pois.rate}
\usage{
  pois.rate(time, r, n = 1000, a, b)
}
\arguments{
  \item{time}{is the total time}

  \item{r}{is the average number of events}

  \item{n}{is the number if simulations required}

  \item{a,b}{are the events being compared}
}
\description{
  Averages the time between any two events in a Poisson
  procces
}
\examples{
pois.rate(100,5,1000,90,100)
## Takes the average time between the 90th and 100th events
}
\author{
  Katie Dodds \email{k.dodds@utah.edu}
}


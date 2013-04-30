\name{pois.proc}
\alias{pois.proc}
\title{pois.proc}
\usage{
  pois.proc(time, r)
}
\arguments{
  \item{time}{is the total time.}

  \item{r}{is the average number of events.}
}
\description{
  Simulates a Poisson process of specified length and rate.
  The output is the time at which each event occured.
}
\examples{
pois.proc(100,5)
}
\author{
  Katie Dodds \email{k.dodds@utah.edu}
}


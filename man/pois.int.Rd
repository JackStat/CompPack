\name{pois.int}
\alias{pois.int}
\title{pois.int}
\usage{
  pois.int(time, r, n = 1000, a)
}
\arguments{
  \item{time}{is the total time.}

  \item{r}{is the average number of events per unit in
  time}

  \item{n}{is the number of simulations required}

  \item{a}{is the time the estimated interval contains}
}
\description{
  Averages the length of the interval containing a
  specified time
}
\examples{
pois.int(100,5,1000,100)
}
\author{
  Katie Dodds \email{k.dodds@utah.edu}
}


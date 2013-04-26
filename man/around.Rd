\name{around}
\alias{around}
\title{Find the Values Around a Particular Value}
\usage{
  around(x, value)
}
\arguments{
  \item{x}{a vector.}

  \item{value}{specified value}
}
\value{
  lo the maximum value of x that is less than or equal to
  the value parameter.

  hi the minimum value of x that is greater than or equal
  to the value parameter.
}
\description{
  Find the location of values around a specified value
}
\examples{
x = rnorm(50, 3, 7)
value = 15
}
\author{
  Tyler Hunt \email{tyler@psychoanalytix.com}
}


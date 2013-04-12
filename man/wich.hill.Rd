\name{wich.hill}
\alias{wich.hill}
\title{Wichman-Hill Random Number Generator}
\usage{
  wich.hill()
}
\description{
  The Wichmann-Hill has a cycle length of \eqn{6.9536e12}.
}
\examples{
replicate(10,wich.hill())
}
\author{
  Tyler Hunt \email{tyler@psychoanalytix.com}
}
\references{
  Wichmann, B. A., & Hill, I. D. (1982). Algorithm AS 183:
  An efficient and portable pseudo-random number generator.
  Journal of the Royal Statistical Society. Series C
  (Applied Statistics), 31(2), 188-190.

  Ripley, B. D. (2009). Stochastic simulation (Vol. 316).
  Wiley.
}


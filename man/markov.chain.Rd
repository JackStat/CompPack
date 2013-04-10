\name{markov.chain}
\alias{markov.chain}
\title{Generate Markov Chains}
\usage{
  markov.chain(p, n)
}
\arguments{
  \item{p}{Probability matrix}

  \item{n}{Number of observations}
}
\description{
  Generate Markov Chains
}
\examples{
A<-matrix(c(.9,.8,0,0,.1,.2,0,0,0,0,.5,.6,0,0,.5,.4), nrow=4)
 B<-matrix(rep(.3,16), nrow=4); diag(B)<-.1
 C<-matrix(c(.9,0,0,0, .1,.9,0,0 ,0,.1,.8,.1, 0,0,.2,.9), nrow=4)
 D<-matrix(c(.9,0,0,.1,.1,.9,0,0,0,.1,.9,0,0,0,.1,.9), nrow=4)
 markov.chain(A, 10)
 markov.chain(B, 10)
 markov.chain(C, 10)
 markov.chain(D, 10)
}
\author{
  Tyler Hunt \email{tyler@psychoanalytix.com}
}


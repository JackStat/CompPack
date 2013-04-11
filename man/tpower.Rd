\name{tpower}
\alias{tpower}
\title{Evaluate Power for a One Sample T-test.}
\usage{
  tpower(reps, n, mu, sd, pval = 0.05)
}
\arguments{
  \item{reps}{the number of simulations.}

  \item{n}{the sample size}

  \item{mu}{a mean}

  \item{sd}{a standard deviation}

  \item{pval}{the criteria for rejection.  Defaults to
  \eqn{p<=.05}}
}
\description{
  Evaluates the power of a one sample t-test and allows
  manipulation of the sample size, the mean, and the
  standard deviation.
}
\author{
  Tyler Hunt \email{tyler@psychoanalytix.com}
}


\name{tpower.data}
\alias{tpower.data}
\title{Matrix of p-values from the tpower function}
\description{
  This is a matrix of p-values from the tpower function.  The columns have different mean differences (See diffs in the example for the differences).
}
\author{
  Tyler Hunt \email{tyler@psychoanalytix.com}
}
\examples{
data(tpower.data)
diffs <- c(0, .05, .15, .2, .25, .3, .35, .4, .45)
#-compute the power at each difference
power<-colMeans(tpower.data<.05)
plot(diffs, power, "l")

}


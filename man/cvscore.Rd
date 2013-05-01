\name{cvscore}
\alias{cvscore}
\title{Cross Validation}
\usage{
  cvscore(x, y)
}
\arguments{
  \item{x}{is the independent variable}

  \item{y}{is the dependent variable}
}
\description{
  Uses cross-validation to score how accurately a a linear,
  quadratic, cubic, and 4th and 5th degree polynomial
  models will perform when used in prediction.
}
\examples{
attach(mtcars)
x=wt
y=mpg
cvscore(x,y)
}
\author{
  Katie Dodds \email{k.dodds@utah.edu}
}


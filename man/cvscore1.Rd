\name{cvscore1}
\alias{cvscore1}
\title{Cross Validation-Linear Model}
\usage{
  cvscore1(x, y)
}
\arguments{
  \item{x}{is the independent variable}

  \item{y}{is the dependent variable}
}
\description{
  Uses cross-validation to score how accurately a linear
  model will perform when used in prediction. Score can be
  compared to that of cvscore2,...,cvscore5.
}
\examples{
attach(mtcars)
x=wt
y=mpg
cvscore1(x,y)
}
\author{
  Katie Dodds \email{k.dodds@utah.edu}
}


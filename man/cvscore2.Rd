\name{cvscore2}
\alias{cvscore2}
\title{Cross Validation-Quadratic Model}
\usage{
  cvscore2(x, y)
}
\arguments{
  \item{x}{is the independent variable}

  \item{y}{is the dependent variable}
}
\description{
  Uses cross-validation to score how accurately a quadratic
  model will perform when used in prediction. Score can be
  compared to that of cvscore1,...,cvscore5.
}
\examples{
attach(mtcars)
x=wt
y=mpg
cvscore2(x,y)
}
\author{
  Katie Dodds \email{k.dodds@utah.edu}
}


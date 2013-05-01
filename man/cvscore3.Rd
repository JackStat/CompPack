\name{cvscore3}
\alias{cvscore3}
\title{Cross Validation-Cubic Model}
\usage{
  cvscore3(x, y)
}
\arguments{
  \item{x}{is the independent variable}

  \item{y}{is the dependent variable}
}
\description{
  Uses cross-validation to score how accurately a cubic
  model will perform when used in prediction. Score can be
  compared to that of cvscore1,...,cvscore5.
}
\examples{
attach(mtcars)
x=wt
y=mpg
cvscore3(x,y)
}
\author{
  Katie Dodds \email{k.dodds@utah.edu}
}


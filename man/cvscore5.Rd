\name{cvscore5}
\alias{cvscore5}
\title{cvscore5}
\usage{
  cvscore5(x, y)
}
\arguments{
  \item{x}{is the independent variable}

  \item{y}{is the dependent variable}
}
\description{
  Uses cross-validation to score how accurately a model
  using a 5th degree polynomial will perform when used in
  prediction. Score can be compared to that of
  cvscore1,...,cvscore4.
}
\examples{
attach(mtcars)
x=wt
y=mpg
cvscore5(x,y)
}
\author{
  Katie Dodds \email{k.dodds@utah.edu}
}


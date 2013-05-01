\name{cvscore4}
\alias{cvscore4}
\title{cvscore4}
\usage{
  cvscore4(x, y)
}
\arguments{
  \item{x}{is the independent variable}

  \item{y}{is the dependent variable}
}
\description{
  Uses cross-validation to score how accurately a model
  using a 4th degree polynomial will perform when used in
  prediction. Score can be compared to that of
  cvscore1,...,cvscore5.
}
\examples{
attach(mtcars)
x=wt
y=mpg
cvscore4(x,y)
}
\author{
  Katie Dodds \email{k.dodds@utah.edu}
}


#' f models
#'
#' @description Plots linear, quadratic, cubic and 4th degree models for a 2D dataset.
#'
#' @param x is the independent variable
#' @param y is the dependent variable
#'
#' @author Katie Dodds \email{k.dodds@@utah.edu}
#'
#' @examples
#' attach(mtcars)
#' f.models(wt,mpg)
#'
#' @export

f.models<-
function (x,y) 
{
  par(mfrow=c(2,2))
  plot(x,y)
  abline(lsfit(x,y),col=2)
  
  plot(x,y)
  z=x*x
  xx=seq(min(x)-10,max(x)+10,.1)
  param=glm(y~x+z)$coeff
  yy=param[1]+xx*param[2]+xx^2*param[3]
  lines(xx,yy,col=3)
  
  plot(x,y)
  w=x*x*x
  param=glm(y~x+z+w)$coeff
  yy=param[1]+xx*param[2]+xx^2*param[3]+xx^3*param[4]
  lines(xx,yy,col=4)

  plot(x,y)
  q=x*x*x*x
  param=glm(y~x+z+w+q)$coeff
  yy=param[1]+xx*param[2]+xx^2*param[3]+xx^3*param[4]+xx^4*param[5]
  lines(xx,yy,col=5)
}

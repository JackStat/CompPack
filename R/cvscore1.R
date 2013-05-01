#' Cross Validation-Linear Model
#'
#' @description Uses cross-validation to score how accurately a linear model will perform when used in prediction. Score can be compared to that of cvscore2,...,cvscore5.
#'
#' @param x is the independent variable
#' @param y is the dependent variable
#'
#' @author Katie Dodds \email{k.dodds@@utah.edu}
#'
#' @examples
#'
#' attach(mtcars)
#' x=wt
#' y=mpg
#' cvscore1(x,y)
#'
#' @export

cvscore1<-
function (x,y) 
{
	n=length(x)
	yhat=rep(0,n)
	
	for (i in 1:n)
	{
		yi=y[-i]
		xi=x[-i]
		zi=xi*yi
		param=glm(yi~xi)$coeff
		yhat[i]=param[1]+x[i]*param[2]
	}
	sum((yhat-y)^2)
}

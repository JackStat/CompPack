#' Cross Validation-Quadratic Model
#'
#' @description  Uses cross-validation to score how accurately a quadratic model will perform when used in prediction. Score can be compared to that of cvscore1,...,cvscore5.
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
#' cvscore2(x,y)
#'
#' @export
cvscore2<-
function (x,y) 
{
	n=length(x)
	yhat=rep(0,n)
	
	for (i in 1:n)
	{
		yi=y[-i]
		xi=x[-i]
		zi=xi*yi
		param=glm(yi~xi+zi)$coeff
		yhat[i]=param[1]+x[i]*param[2]+x[i]^2*param[3]		
	}
	sum((yhat-y)^2)
}

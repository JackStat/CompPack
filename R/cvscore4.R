#' cvscore4
#'
#' @description Uses cross-validation to score how accurately a model using a 4th degree polynomial will perform when used in prediction. Score can be compared to that of cvscore1,...,cvscore5.
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
#' cvscore4(x,y)
#'
#' @export
cvscore4<-
function (x,y) 
{
	n=length(x)
	yhat=rep(0,n)
	
	for (i in 1:n)
	{
		yi=y[-i]
		xi=x[-i]
		zi=xi*xi
		wi=xi*xi*xi
		vi=xi*xi*xi*xi
		param=glm(yi~xi+zi+wi+vi)$coeff
		yhat[i]=param[1]+x[i]*param[2]+x[i]^2*param[3]	+x[i]^3*param[4]	+x[i]^4*param[5]
	}
	sum((yhat-y)^2)
}
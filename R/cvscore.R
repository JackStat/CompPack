#' cvscore
#'
#' @description Uses cross-validation to score how accurately a a linear, quadratic, cubic, and 4th and 5th degree polynomial models will perform when used in prediction.
#'
#' @param x is the independent variable
#' @param y is the dependent variable
#'
#' @author Katie Dodds \email{k.dodds@@utah.edu}
#'
#' @examples
#' attach(mtcars)
#' x=wt
#' y=mpg
#' cvscore(x,y)
#'
#' @export
cvscore<-
function(x,y){
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
	linear=sum((yhat-y)^2)
}
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
	quadratic=sum((yhat-y)^2)
}
{
	n=length(x)
	yhat=rep(0,n)
	
	for (i in 1:n)
	{
		yi=y[-i]
		xi=x[-i]
		zi=xi*xi
		wi=xi*xi*xi
		param=glm(yi~xi+zi+wi)$coeff
		yhat[i]=param[1]+x[i]*param[2]+x[i]^2*param[3]	+x[i]^3*param[4]	
	}
	cubic=sum((yhat-y)^2)
} 
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
	fourth.degree=sum((yhat-y)^2)
}
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
		ui=xi^5
		param=glm(yi~xi+zi+wi+vi+ui)$coeff
		yhat[i]=param[1]+x[i]*param[2]+x[i]^2*param[3]	+x[i]^3*param[4]	+x[i]^4*param[5]+x[i]^5*param[6]
	}
	fifth.degree=sum((yhat-y)^2)
}
list(linear=linear, quadratic=quadratic, cubic=cubic, fourth.degree, fifth.degree)
}

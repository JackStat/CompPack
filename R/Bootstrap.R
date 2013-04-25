#' Bootstrap
#' 
#' @description
#' This function is used for estimating standard errors when the distribution is not know.
#' 
#' @param x a vector.
#' @param Boots The number of bootstraps.
#' @param fn the function you want to bootstrap, ie., mean, var, cov, etc.
#' 
#' @author Tyler Hunt \email{tyler@@psychoanalytix.com}
#' 
#' @examples
#' x = runif(10, 0, 1)
#' Bootstrap(x,fn=mean)
#' 
#' @export


Bootstrap<-function(x, Boots=100, fn){
	n=length(x)
	lings<-replicate(Boots, fn(sample(x,n, replace=TRUE)))
	
	list(se=sd(lings), lings=lings)
}

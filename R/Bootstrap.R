#' Bootstrap
#' 
#' @param x a vector.
#' @param Boots The number of bootstraps
#' @param fn the function you want to bootstrap
#' 
#' @author Tyler Hunt \email{tyler@@psychoanalytix.com}
#' 
#' @export


Bootstrap<-function(x, Boots=100, fn){
	n=length(x)
	lings<-replicate(Boots, fn(sample(x,n, replace=TRUE)))
	
	list(se=sd(lings), lings=lings)
}

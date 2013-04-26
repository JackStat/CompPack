#' Grid Section Search Optimization
#' 
#' @description method of numerical optimization which uses a simple learning algorythm that compares two points of a function. The lower of the two points is discarded, then adjusted and re-evaluated. The process is repeated until an optimum is located. 
#' 
#' @param f the function to be optimized
#' @param lo the lower bound
#' @param hi the upper bound
#' 
#' @author Tyler Hunt \email{tyler@@psychoanalytix.com}
#' 
#' @export

grid.sect <-
function(f,lo,hi){
	l=lo
	h=hi
	while(h-l>.0000001){
		x=(0:1000)/1000*(h-l)+l
		x=seq(l,h,(h-l)/1000)
		y=f(x)
		m=max(y)
		i = (1:length(x))[y==m]
		i=i[(1+length(i))/2]
		l=x[i-1]
		h=x[i+1]
	}
	plot(x,y, "l")
	lines(c(x[i],x[i]), c(-100, 100), col=2)
	list(x[i])
}

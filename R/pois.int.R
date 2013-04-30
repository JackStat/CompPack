#' pois.int
#'
#' @description Averages the length of the interval containing a specified time
#'
#' @param time is the total time.
#' @param r is the average number of events per unit in time
#' @param n is the number of simulations required
#' @param a is the time the estimated interval contains
#'
#' @author Katie Dodds \email{k.dodds@@utah.edu}
#'
#' @examples
#'
#' pois.int(100,5,1000,100)
#'
#' @export
pois.int<-
function(time,r,n=1000,a){
	z=0
	for(i in 1:n){
		x=pois.proc(time,r)
		y=min(x[x>a])-max(x[x<a])
		z=z+y
	}
	z/n
}

#' pois.rate
#'
#' @description Averages the time between any two events in a Poisson procces
#'
#' @param time is the total time
#' @param r is the average number of events
#' @param n is the number if simulations required
#' @param a,b are the events being compared
#'
#' @author Katie Dodds \email{k.dodds@@utah.edu}
#'
#' @examples
#'
#' pois.rate(100,5,1000,90,100)
#' ## Takes the average time between the 90th and 100th events
#'
#' @export
pois.rate<-
function(time,r,n=1000,a,b){
	z=0
	for(i in 1:n){
		x=pois.proc(time,r)
		y=x[b]-x[a]
		z=z+y
	}
	abs(z/n)
}

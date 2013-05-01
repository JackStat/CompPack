#' pois.proc
#'
#' @description Simulates a Poisson process of specified length and rate. The output is the time at which each event occured.
#'
#' @param time is the total time.
#' @param r is the average number of events.
#'
#' @author Katie Dodds \email{k.dodds@@utah.edu}
#'
#' @examples
#'
#' pois.proc(100,5)
#'
#' @export

pois.proc=
function(time,r){
	t=0
	x=0
	while(t<time){
		t=t+rexp(1,r)
		x=c(x,t)
	}
	x
}

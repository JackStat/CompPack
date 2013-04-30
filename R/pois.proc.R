<<<<<<< HEAD
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
=======
#' Generate a Poisson Process
#' 
#' @description
#' Generates a Poisson Process.
#' 
#' @param end the end time desired.
#' @param rate the rate of occurrence.
#' 
#' @author Tyler Hunt \email{tyler@@psychoanalytix.com} 
#' 
#' @results cumsum the cumulative sum of the vector of a poisson process.
#' 
#' @examples pois.proc(200, 1)
>>>>>>> 72ae3e5c0dc4c43e0c46577ad8f2a4f1363a0657
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

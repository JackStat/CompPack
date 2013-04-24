#' Generate a Poisson Process
#' 
#' @param end the end time desired.
#' @param rate the rate of occurrence.
#' 
#' @author Tyler Hunt \email{tyler@@psychoanalytix.com} 
#' 
#' @examples pois.proc(200, 1)
#' @export

pois.proc <- function(end=200, rate=1){
  mm<-c()
  while(sum(mm)<end){
    val<-rand.exp(1, rate)
    mm<-c(mm, val)
  }
  cumsum(mm[-length(mm)])
}

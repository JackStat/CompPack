#' Generate Random Exponential Variables
#' 
#' @param n Number of observations
#' @param rate vector of rates
#' 
#' @author Tyler Hunt \email{tyler@@psychoanalytix.com}
#' 
#' @export

rand.exp<-function(n, rate=1){
  qexp( replicate( n, wich.hill() ) )*rate
}

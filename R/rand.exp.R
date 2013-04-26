#' Generate Random Exponential Variables
#' 
#' @description 
#' Generates a pseudo-random exponential variable using the Whichman-Hill pseudo-random number generator function
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

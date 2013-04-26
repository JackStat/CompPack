#' Generate Random Uniform Variables
#' 
#' @description
#' Generates pseudo-random variables from a Uniform distribution, using the Wichmannhill.
#' 
#' @param n Number of observations.
#' @param min lower limit of the distribution.
#' @param max upper limit of the distribution.
#' 
#' @author Tyler Hunt \email{tyler@@psychoanalytix.com}
#' 
#' @export

rand.unif<-function(n, min=0, max=1){
  min + (max-min) * replicate( n , wich.hill() )
}

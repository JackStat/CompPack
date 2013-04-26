#' Generate Random Normal Variables
#' 
#' @description
#' Generates pseudo random variables from a normal distribution using the Wichmannhill.
#'
#' @param n Number of observations.
#' @param mean vector of means.
#' @param sd vector of standard deviations.
#' 
#' @author Tyler Hunt \email{tyler@@psychoanalytix.com}
#' 
#' @export

rand.norm<-function(n, mean, sd){
  qnorm( replicate( n, wich.hill() ) )*sd + mean
}

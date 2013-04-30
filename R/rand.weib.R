#' Generate Random Weibull Variables
#' 
#' @param n the number of observations
#' @param beta the specification of beta
#' @param theta the specification of theta
#' 
#' @author Tyler Hunt \email{tyler@@psychoanalytix.com}
#' 
#' @export

rand.weib<-function(n, beta, theta){
  x<-rand.unif(n, 0, 1)
  theta*(log(1/x)^(1/beta))
}
#' Generate Multivariate Normal Distribution
#' 
#' @description A method for generating random multivariate normal data that is based on a cholesky decomposition.
#' @param n the number of observations
#' @param mu a vector of means
#' @param sigma a variance-covariance matrix
#' 
#' @author Tyler Hunt \email{tyler@@psychoanalytix.com}
#' 
#' @examples
#' sigma=matrix(rep(.5, 9), nrow=3)
#' diag(sigma)<-1
#' mvrand.norm(100, c(1,5,11), sigma)
#' 
#' @export

mvrand.norm<-function(n, mu, sigma){
  k<-length(mu)
  vs<-matrix(rep(NA, k*n), ncol=k)
  for(i in 1:n){
    vs[i,]<-rnorm(k,mu,1)
    vs[i,]<-t(chol(sigma))%*%vs[i,]
  }
  vs
}

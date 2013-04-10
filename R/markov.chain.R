#' Generate Markov Chains
#' 
#' 
#' @param p Probability matrix
#' @param n Number of observations
#' 
#' @author Tyler Hunt \email{tyler@@psychoanalytix.com}
#' 
#' @examples
#'  A<-matrix(c(.9,.8,0,0,.1,.2,0,0,0,0,.5,.6,0,0,.5,.4), nrow=4)
#'  B<-matrix(rep(.3,16), nrow=4); diag(B)<-.1
#'  C<-matrix(c(.9,0,0,0, .1,.9,0,0 ,0,.1,.8,.1, 0,0,.2,.9), nrow=4)
#'  D<-matrix(c(.9,0,0,.1,.1,.9,0,0,0,.1,.9,0,0,0,.1,.9), nrow=4)
#'  markov.chain(A, 10)
#'  markov.chain(B, 10)
#' @export
  markov.chain<-function(p, n){
    
    k<-nrow(p)
    start<-sample(1:k, 1)
    
    x<-rep(NA, n)
    for(i in 1:n){
      x[i]<-sample(1:k, 1, replace=FALSE, prob=p[start,])
      start=x[i]
    }
    x
  }
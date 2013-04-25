#' Generate A Random Normal Variable
#' 
#' @description A method for generating a random normal variable based upon the polar method.
#' @param mu mean of a normal distribution
#' @param sigma variance of a normal distribution
#' 
#' @author Damon McCafferty \email{damon.mccafferty@@economics.utah.edu}
#' 
#' @export

polar.norm<-function(mu, sig)
{
  
  u<-(2*wich.hill()-1)
  v<-(2*wich.hill()-1)
  
  s<-u^2+v^2
  
  while(s==0||s>=1)
  {
    u<-(2*wich.hill()-1)
    v<-(2*wich.hill()-1)
    
    s<-u^2+v^2 
  }
  sig*sqrt(-2*log(s)/s)*u + mu    
}

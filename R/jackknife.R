#' Resamples Data using the Jackknife Method
#' 
#' @description 
#' a resampling method that can be used to estimate the bias and variance when a random sample of observations is used.
#'  
#' @param x a vector
#' @param t estimation of parameter
#' 
#' @return est orignial estimation of parameter
#' @return jkest jackknife estimation of parameter
#' @return jkvar jackknife estimation of variance
#' @return jkbias jackknife estimate of biasness of parameter
#' @return jkbiascorr bias corrected parameter estimate
#' 
#' @author Damon McCafferty \email{damon.mccafferty@@economics.utah.edu}
#' 
#' @examples x = runif(10, 0, 1)
#' mean(x)
#' jackknife(x,mean)
#' 
#' @export

jackknife<-function (x,t) 
{
  n=length(x)
  jk=rep(NA,n)
  
  for (i in 1:n)
    {
    jk[i]=t(x[-i])
  jkest=mean(jk)
  jkvar=(n-1)/n*sum((jk-jkest)^2)
  jkbias=(n-1)*(jkest-t(x))
  jkbiascorr=n*t(x)-(n-1)*jkest
    }
  list(est=t(x), jkest=jkest, jkvar=jkvar, jkbias=jkbias, jkbiascorr=jkbiascorr)
}

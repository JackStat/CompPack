#' Polar Random Normal
#'
#' @description Generates a sample of standard normals
#'
#' @param n is the sample size
#' @param mu a vector of means
#' @param sd a vector of standard deviations
#'
#' @author 
#' Katie Dodds \email{k.dodds@@utah.edu}
#' Damon McCafferty \email{damon.mccafferty@@economics.utah.edu}
#'
#' @examples
#'
#' polar.rnorm(100) 
#' 
#' @export

polar.rnorm<-
function (n=100, mu=0, sd=1)
{
	theta=2*pi*runif(n)
	r=sqrt(-2*log(runif(n)))
	
	r*cos(theta)*sd+mu
}

#' Second Derivative
#' 
#' @description
#' The Second derivative of a function evaluated at point(s) x.
#' 
#' Roughly spoken, the seond derivative is the derivative of a derivative. This can be used to measure how the rate of change of a quantitty is itself changing.
#' 
#' @param f The function that you want to take the derivative of
#' @param x The point or vector of points where the derivative will be evaluated
#' 
#' @author Damon McCafferty \email{damon.mccafferty@@economics.utah.edu}
#' 
#' @examples
#' x = runif(10, pi/3, 3*pi/4)
#' df(tan, x)
#' 
#' @export

df <- function(f,x)
{
  d=0.0000001
  (f(x-d)-2*f(x))/d/d  
}

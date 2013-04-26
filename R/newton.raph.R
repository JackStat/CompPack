#' Newton Raphson Alogorithm
#' 
#' @description
#' A method for locating succesively better approximations for the optimal point of a function. The Newton Raphson method is named after Sir Issac Newton and Joseph Raphson
#' 
#' @param f The function that you want to optimize.
#' @param df the 1st derivative of the function.
#' @param d2f the 2nd derivative of the function.
#' @param start The starting value for optimization.
#' @param tol The level of tolerance desired for convergence.
#' 
#' @author Tyler Hunt \email{tyler@@psychoanalytix.com}
#' @export


newton.raph <- function(f, df, d2f, start, tol=0.000001){
  
  new <- start + 10*tol
  iter <- 0
  while(abs(start - new) > tol){
    iter <- iter+1
    new <- start
    start <- start - df(start)/d2f(start)
  }
  result<-c(Estimate=start, Iterations=iter, Likelihood=f(start))
  return(result)
}

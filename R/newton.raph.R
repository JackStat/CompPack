#' Newton Raphson Alogorithm
#' 
#' @description
#' A method for obtaining successively better approximations for the  optimal value of a function. Named after Sir Isaac Newton and Joseph Raphson.
#' 
#' @param f The function that you want to optimize.
#' @param d1f the 1st derivative of the function.
#' @param d2f the 2nd derivative of the function.
#' @param start The starting value for optimization.
#' @param tol The level of tolerance desired for convergence.
#' 
#' @return Estimate Initial estimate of the optimum value.
#' @return Iterations Number of cycles used to obtain the optimum value of the function.
#' @return Liklihood Optimum value
#' 
#' @author Tyler Hunt \email{tyler@@psychoanalytix.com}
#' @export


newton.raph <- function(f, d1f, d2f, start, tol=0.000001){
  
  new <- start + 10*tol
  iter <- 0
  while(abs(start - new) > tol){
    iter <- iter+1
    new <- start
    start <- start - d1f(start)/d2f(start)
  }
  result<-c(Estimate=start, Iterations=iter, Likelihood=f(start))
  return(result)
}

#' Find the Values Around a Particular Value
#' 
#' @description Find the location of values around a specified value
#' 
#' @param x a vector.
#' @param value specified value
#' 
#' @author Tyler Hunt \email{tyler@@psychoanalytix.com} 
#' 
#' @return lo the maximum value of x that is less than or equal to the value parameter.
#' @return hi the minimum value of x that is greater than or equal to the value parameter.
#' 
#' @examples
#' x = rnorm(50, 3, 7)
#' value = 15
#' 
#' @export 

around<-function(x, value){
  x<-sort(x)
  lo<-x[nearest.loc(x, value)]
  if(lo>=value)
    lo<-x[nearest.loc(x, value)-1]
  
  hi<-x[nearest.loc(x, value)]
  if(hi<value)
    hi<-x[nearest.loc(x, value)+1]
  
  c(lo, hi)
}

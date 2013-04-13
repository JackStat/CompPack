#' Find the Values Around a Particular Value
#' 
#' @description Find the location of values around a specified value
#' 
#' @param x a vector, matrix, or data frame.
#' @param value specified value
#' 
#' @author Tyler Hunt \email{tyler@@psychoanalytix.com} 
#' @export

around<-function(x, value){
  x<-sort(x)
  lo<-x[nearest(x, value)]
  if(lo>=value)
    lo<-x[nearest(x, value)-1]
  
  hi<-x[nearest(x, value)]
  if(hi<value)
    hi<-x[nearest(x, value)+1]
  
  c(lo, hi)
}

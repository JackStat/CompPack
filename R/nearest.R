#' Find the Nearest Value
#' 
#' @description Find the the nearest value to a number that you specify.
#' 
#' @param x a vector.
#' @param value the value that you want to find.
#' 
#' 
#' @author Tyler Hunt \email{tyler@@psychoanalytix.com} 
#' @export

nearest<-function(x, value){
  nearloc<-nearest.loc(x, value)
  x[nearloc]
}

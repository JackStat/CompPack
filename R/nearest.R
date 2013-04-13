#' Find Nearest Value
#' 
#' @description Find the nearest value to a number that you specify.
#' 
#' @param x a vector or matrix
#' @param value the value you want
#' 
#' 
#' @author Tyler Hunt \email{tyler@@psychoanalytix.com} 
#' @export

nearest<-function(x, value){
  which(abs(x-value)==min(abs(x-value)))
}

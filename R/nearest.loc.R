#' Find Location of Nearest Value
#' 
#' @description Find the location of the nearest value to a number that you specify.
#' 
#' @param x a vector.
#' @param value the value that you want to find.
#' 
#' 
#' @author Tyler Hunt \email{tyler@@psychoanalytix.com} 
#' @export

nearest.loc<-function(x, value){
  which(abs(x-value)==min(abs(x-value)))
}

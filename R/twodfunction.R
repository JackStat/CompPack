#' cos(x) + cos(y)
#' 
#' @description 
#' Generate cos(x) + cos(y) maxtrix
#' 
#' @param x vector of value
#' @param y vector of value
#' 
#' @examples
#' twodfunction(1,1)
#' 
#' @author Ernest Chan \email{faiernest@@gmail.com}
#' @export
twodfunction = function(x,y)
{
  z = matrix(0, nrow=length(x), ncol=length(y))
  for(i in 1:length(y))
  {
    for(j in 1: length(x))
    {
      z[i,j] = cos(x[j]) + cos(y[i])
    }
  }
  z
}

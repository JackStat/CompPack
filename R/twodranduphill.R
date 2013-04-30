#' twodranduphill
#' 
#' @description
#' This function is use to do a random uphill search on any 2 demision data set
#' 
#' @param f matrix of data set
#' @param y is the starting point for the search
#' @param x is the starting point for the search
#' @param n is the number of iteration for the search
#' @param s standard devation for each search jump, default at 1
#' @param t is the tempearture, where its default at 100
#' 
#' @example
#' twodranduphill(matrix(c(1,2,3,4,5,6,7,8,9,10,.1,.2,5,3,7,6), ncol = 4), 1, .4, 1, 100, 10)
#' 
#' @author Ernest Chan \email{faiernest418@@gmail.com}
#' 
#' @export

twodranduphill = function(f, x, y, s, n=100, t=100)
{
  
  fm = f(x, y)
  
  resx = rep(0,n+1)
  resy = rep(0,n+1)
  resf = rep(0,n+1)
  resx[1] = x
  resy[1] = y
  resf[1] = fm
  
  for( i in 1:n)
  {
    newx = x + rnorm(1, 0, s)
    newy = y + rnorm(1, 0, s)
    newfm = f(newx, newy)
    
    if(1 < (newfm/fm)^(1/t))
    {
      x = newx
      y = newy
      fm = newfm
    }
    resx[i+1] = x
    resy[i+1] = y
    resf[i+1] = fm
    
    t = t * 0.999
    s = s * 0.9999
  }
  list(x = resx, y = resy, z = resf)	
  
}

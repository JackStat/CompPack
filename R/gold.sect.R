#' Golden Section Search Optimization
#'
#' @param f The function to be optimized
#' @param lo Lower estimate
#' @param hi Upper estimate
#' @param tol Level of tolerance desired for optimization
#' 
#' @return a estimated optimal value of the function
#' @return f(a) function value at the optimal value
#'
#' @author Tyler Hunt \email{tyler@@psychoanalytix.com}
#' 
#' @examples
#' gold.sect(sin, 1/2, pi/2)
#'
#' @export

gold.sect <- function(f,lo,hi,tol=0.0000001){
	g = 1-(sqrt(5)-1)/2
	h =	 hi
	l = lo
	a = l + g * (h-l)
	fa = f(a)
	b = a + g * (h-a)
	fb = f(b)
	while (h - l > tol){
		if (fa > fb){
			h = b
			b = a
			fb = fa
			a = b - g*(b-l)
			fa = f(a)
			}
	else{
		l = a
		a = b
		fa = fb
		b = a + g*(h-a)
		fb = f(b)
		}
	}
c(a,fa)
}

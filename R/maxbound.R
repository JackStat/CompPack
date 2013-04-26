#' Maxbound Optimization
#' 
#' @description
#' Used to estimate the bounds of a function, in which the optimum value will be located within.
#' 
#' @param f the function to be optimized.
#' @param lo the lower bound.
#' @param hi the upper bound.
#' 
#' @return lo lower bound estimate of the function's optimum.
#' @return hi upper bound estimate of the function's optimum.
#' 
#' @author Tyler Hunt \email{tyler@@psychoanalytix.com}
#' 
#' @examples
#' maxbound(sin, 1/2, pi/2)
#' 
#' 
#' @export

maxbound <-
function(f,lo,hi)
{
	l = lo
	fl = f(l)
	h = hi
	fh = f(h)
	a = l + 0.5*(h-l)
	fa = f(a)
	while (fa < fl || fa < fh)
	{
		if (fa < fh)
		{
			a = h
			fa = fh
			h = a + 2*(a-l)
			fh = f(h)
		}
		else
		{	
			a = l
			fa = fl
			l = a - 2*(h-a)
			fl = f(l)
		}
	}
c(l,h)
}

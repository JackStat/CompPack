#' mixlike
#' 
#' @description
#' A supplement fucntion for uphill.mixnorm, this use to compute the log likelihood funciotn for the mixture of normal distribution
#' 
#' @param x is the mixture function to be evaluated
#' @param p is the vector of probabilities
#' @param m is the vector if means
#' @param s is the vector of standard deviation
#' 
#' @examples
#' mixlike(mixnorm(100, c(.50, .15, .35), c(-3, 0, 3), c(3,3,3)), c(.5,.5), c(-5,5), c(1,1))
#' 
#' @author Ernest Chan \email{faiernest418@@gmail.com}
#' 
#' @export

mixlike = function(x, p, m, s)
{
	ll = 0

	for (i in 1:length(p))
	{
		fi = 0

		for (j in 1:length(p))
		{
			fi =  fi + p[j] * dnorm(x[i], m[j], s[j])
		}

		ll = ll + log(fi)
	}

	ll

}

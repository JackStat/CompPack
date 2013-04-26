#' EM-Algorithm for Normal Distribution
#'
#' @param x the data.
#' @param k estimate of the number of mixtures.
#' 
#' @return itterations number of cycles the program used to calculate the estimated means, standard devaitions and probabilities.
#' @return means estimated mean of each mixture.
#' @return stddevs estimated standard deviation of each mixture.
#' @return probs estimated probabilities of each mixture.
#' 
#' @author Tyler Hunt \email{tyler@@psychoanalytix.com}
#' 
#' @examples
#' x=mixnorm(100, c(.50, .15, .35), c(-3, 0, 3), c(3,3,3))
#' em.mixnorm(x,3)
#' 
#' @export

em.mixnorm <-
function (x,k){
	p = rep(1/k,k)
	s = rep(1,k)

	mnx = min(x)
	mxx = max(x)
	m = (1:k) * (mxx-mnx)/(k+1) + mnx

	y = matrix(0,nrow=length(x),ncol=k)

	err = 100;
	its = 0;

	while (err > 0.001)
	{
		oldm = m
		olds = s
		oldp = p

		its = its+1

		# E-step

		for (i in 1:nrow(y))
		{
			for (j in 1:ncol(y))
				y[i,j] = p[j] * dnorm(x[i],m[j],s[j])
	
			y[i,] = y[i,] / sum( y[i,] )
		}
	
		# M-step
	
		for (j in 1:length(m))
		{
			m[j] = sum(x * y[,j])/sum(y[,j])
			s[j] = sum( (x-m[j])^2 * y[,j]) / sum(y[,j])
			s[j] = sqrt(s[j])
			p[j] = sum(y[,j])/sum(y)
		}

		err = sum(abs(oldm-m) + abs(olds-s) + abs(oldp-p))
	}

	list(iterations=its, means=m, stddevs=s, probs=p)
}

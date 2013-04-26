#' Create a Mixture of Normal Distributions
#'
#' @description
#' Creates a mixture of normal distributions.
#' 
#' @param n number of observations. If \eqn{length(n) > 1}, the length is taken to be the number required.
#' @param p vector of probabilities.
#' @param mu vector of means.
#' @param sd vector of standard deviations.
#'
#' @author Tyler Hunt \email{tyler@@psychoanalytix.com}
#' 
#' @examples
#' x = mixnorm(100, c(.50, .15, .35), c(-3, 0, 3), c(3,3,3))
#' hist(x)
#' 
#' @export

mixnorm <-
function(n, p, mu, sd){
		z <- sample(length(p), n, replace = TRUE, prob = p)
		rnorm(n, mu[z], sd[z])
}

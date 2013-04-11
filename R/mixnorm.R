#' Create a Mixture of Normal Distributions
#'
#' @param n number of observations. If \eqn{length(n) > 1}, the length is taken to be the number required
#' @param p vector of probabilities
#' @param mu vector of means
#' @param sd vector of standard deviations
#'
#' @author Tyler Hunt \email{tyler@@psychoanalytix.com}
#' @export

mixnorm <-
function(n, p, mu, sd){
		z <- sample(length(p), n, replace = TRUE, prob = p)
		rnorm(n, mu[z], sd[z])
}

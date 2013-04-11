#' Evaluate Power for a One Sample T-test.
#' 
#' @description Evaluates the power of a one sample t-test and allows manipulation of the sample size, the mean, and the standard deviation.
#' 
#' @param reps the number of simulations.
#' @param n the sample size
#' @param mu a mean
#' @param sd a standard deviation
#' @param pval the criteria for rejection.  Defaults to \eqn{p<=.05}
#' 
#' @author Tyler Hunt \email{tyler@@psychoanalytix.com}
#' 
#' @export



tpower<-function(reps, n, mu, sd, pval=.05){
  pv<-replicate(reps,t.test(rand.norm(n, mu, sd))$p.value)
  mean(pv<pval)
}

#' Wichman-Hill Random Number Generator
#' 
#' @description hi
#' 
#' @author Tyler Hunt \email{tyler@@psychoanalytix.com} 
#' 
#' @references
#'  Wichmann, B. A., & Hill, I. D. (1982). Algorithm AS 183: An efficient and portable pseudo-random number generator. Journal of the Royal Statistical Society. Series C (Applied Statistics), 31(2), 188-190.
#'  
#'  Ripley, B. D. (2009). Stochastic simulation (Vol. 316). Wiley.
#'  
#'  @examples
#'  replicate(10,wich.hill())
#'  
#' @export

wich.hill <-
function(){

	if(!exists(".wh.seed")){ 
    .wh.seed<<-c(1,1,1)
	}
		
	.wh.seed[1]<<-171 * (.wh.seed[1] %% 30269) 
	.wh.seed[2]<<-172 * (.wh.seed[2] %% 30307) 
	.wh.seed[3]<<-170 * (.wh.seed[3] %% 30323)
	
	x<- (.wh.seed[1] / 30269) + (.wh.seed[2] / 30307) + (.wh.seed[3] / 30323)
	x - floor(x)
}

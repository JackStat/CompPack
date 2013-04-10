#' Wichman-Hill Random Number Generator
#' 
#' 
#' @author Tyler Hunt \email{tyler@@psychoanalytix.com} 
#' @export

wich.hill <-
function(){

	if(!exists(".wh.seed")){ 
    .wh.seed<-c(1,1,1)
	}
		
	.wh.seed[1]<<-171 * (.wh.seed[1] %% 30269) 
	.wh.seed[2]<<-172 * (.wh.seed[2] %% 30307) 
	.wh.seed[3]<<-170 * (.wh.seed[3] %% 30323)
	
	x<- (.wh.seed[1] / 30269) + (.wh.seed[2] / 30307) + (.wh.seed[3] / 30323)
	x - floor(x)
}

#' Update CompPack
#' 
#' @author Tyler Hunt \email{tyler@@psychoanalytix.com} 
#' @export

update.CompPack<-function(){
  require(devtools)
  install_github("CompPack", "JackStat")
  require(CompPack)
}

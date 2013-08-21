#' @title summary
#' 
#' @description Summarize WAR
#' 
#' @details Summary of players' WAR
#' 
#' @param An object of class \code{"WAR"}
#' 
#' 
#' 
#' 
#' @examples
#' 
#' ds = getData()
#' out = (makeWAR(ds))
#' summary(out)

summary.war = function (data, ...) {
  require(plyr)  
  players = ddply(data, ~Name, summarise, q0 = min(RAA), q2.5 = quantile(RAA, 0.025), q50 = mean(RAA), q97.5 = quantile(RAA, 0.975), q100 = max(RAA))
  return(players[order(players$q50, decreasing=TRUE),])
}

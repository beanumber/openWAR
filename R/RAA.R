#' @title RAA
#' 
#' @description Contains the output from getWAR()
#' 
#' 
#' @examples showClass("RAA")

setClass("RAA", contains = "data.frame")

#' @title summary.RAA
#' 
#' @description Summarize Runs Above Average among players
#' 
#' @details A Summary of players' WAR
#' 
#' @param data An object of class \code{"RAA"}
#' 
#' @export summary.RAA
#' @examples
#' 
#' ds = getData()
#' out = (makeWAR(ds))
#' summary(out)

summary.RAA = function (data, ...) {
  require(plyr)  
  players = ddply(data, ~Name, summarise, q0 = min(RAA), q2.5 = quantile(RAA, 0.025), q50 = mean(RAA), q97.5 = quantile(RAA, 0.975), q100 = max(RAA))
  return(players[order(players$q50, decreasing=TRUE),])
}

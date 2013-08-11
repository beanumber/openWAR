#' @title summary.war
#' 
#' @description Summarize WAR
#' 
#' @details Aggegrate the resampled WAR estimates
#' 
#' @param playerIds A vector of valid MLBAM player IDs present in the data argument
#' @param data A dataset of resampled WAR components, aggregated by player
#' 
#' @return a data.frame
#' 
#' @export
#' @examples
#' 
#' ds = getData()
#' samp = shakeWAR(makeWAR(ds))
#' summary.war(samp)

summary.war = function (data, ...) {
  require(plyr)  
  players = ddply(data, ~Name, summarise, q0 = min(RAA), q2.5 = quantile(RAA, 0.025), q50 = mean(RAA), q97.5 = quantile(RAA, 0.975), q100 = max(RAA))
  return(players[order(players$q50, decreasing=TRUE),])
}

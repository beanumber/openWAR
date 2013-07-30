#' @title warplot
#' 
#' @description Visualize WAR
#' 
#' @details Density Plot for WAR estimates
#' 
#' @param playerIds A vector of valid MLBAM player IDs present in the data argument
#' @param data A dataset
#' @param N the number of bootstrapped estimates
#' 
#' @return an xyplot() 
#' 
#' @export
#' @examples
#' 
#' ds = getData()
#' warplot(ds)

warplot = function (playerIds, data, N = 5000, ...) {
  require(mosaic)
  bgcol = "darkgray"
  playerIds = sort(playerIds)
  # is it worth the trouble to filter the rows? 
  #  rows = subset(data, batterId %in% playerIds | start1B %in% playerIds | start2B %in% playerIds | start3B %in% )
  # Remove unused factor levels
#  data$batterName = factor(rows$batterName)
  lkup = unique(data[,c("batterId", "batterName")])
  lkup = subset(lkup, batterId %in% playerIds)
  labels = as.character(lkup[order(lkup$batterId),]$batterName)
  bstraps = lapply(playerIds, getWARest, data = data, N = N)
  X = do.call("rbind", bstraps)
  X = merge(x=X, y=lkup, by.x="playerId", by.y="batterId")
  
  plot = densityplot(~result | component, groups=playerId, data=X
         , panel = function(x,y,...) {
           panel.densityplot(x, plot.points=FALSE, ...)
           if (length(bstraps) == 1) {
             quants = qdata(c(0.05, 0.5, 0.975), vals=x)
             h = 0.025
             eps = 0.05 * diff(range(x))
             panel.abline(v=quants[1], col=bgcol)
             panel.abline(v=quants[2], col=bgcol)
             panel.abline(v=quants[3], col=bgcol)
             panel.text(quants[2], h, paste("Point Estimate:\n", round(quants[2],2)))
             panel.text(quants[1], h/3, paste(round(quants[1],2)))
             panel.text(quants[3], h/3, paste(round(quants[3],2)))
             panel.arrows(quants[1] + eps, h/3, quants[3] - eps, h/3, ends = "both")
             panel.text(quants[2], h/2, "95% CI")
           }
         }
       , auto.key=list(columns=min(4, length(playerIds)), text = labels)
       , ylim = c(-0.01, 0.2)
       , xlab = "Runs Above Average (RAA)"
  )
  return(plot)
}

getWARest = function (playerId, data, ...) {
  delta = list("Batting" = NULL, "Baserunning" = NULL, "Pitching" = NULL, "Fielding" = NULL)
  delta$Batting = subset(data, batterId == playerId)$raa.bat
  delta$Baserunning = c(subset(data, start1B == playerId)$raa.br1
               , subset(data, start2B == playerId)$raa.br2, subset(data, start3B == playerId)$raa.br3)
  delta$Pitching = subset(data, pitcherId == playerId)$raa.pitch
  delta$Fielding = c(subset(data, pitcherId == playerId)$raa.P, subset(data, playerId.C == playerId)$raa.C
               , subset(data, playerId.1B == playerId)$raa.1B, subset(data, playerId.2B == playerId)$raa.2B
               , subset(data, playerId.3B == playerId)$raa.3B, subset(data, playerId.SS == playerId)$raa.SS
               , subset(data, playerId.LF == playerId)$raa.LF, subset(data, playerId.CF == playerId)$raa.CF
             , subset(data, playerId.RF == playerId)$raa.RF)
  raa = lapply(delta, bstrap)
  raa$RAA = data.frame(result = with(raa, ifelse(is.null(Batting), 0, Batting)))
  raa$RAA = with(raa, ifelse(is.null(Baserunning), RAA, RAA + Baserunning))
  raa$RAA = with(raa, ifelse(is.null(Pitching), RAA, RAA + Pitching))
  raa$RAA = data.frame(result = with(raa, ifelse(is.null(Fielding), RAA, RAA + Fielding)))
  names(raa$RAA) = c("result")
  ns = unlist(sapply(raa, nrow))
  out = data.frame(playerId = playerId, component = rep(names(ns), ns), raa = do.call("rbind", raa))
  return(out)
}

bstrap = function (x, N = 5000) {
  if (length(x) > 0) {
    return(do(N) * sum(resample(x), na.rm=TRUE))
  } else {
    return(NULL)
  }
}

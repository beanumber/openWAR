#' @title warplot
#' 
#' @description Visualize WAR
#' 
#' @details Density Plot for WAR estimates
#' 
#' @param batterId A vector of valid MLBAM player ID present in the data argument
#' @param data 
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
       , xlab = "Runs Above Average (RAA)"
  )
  return(plot)
}

getWARest = function (playerId, data, ...) {
  delta = list()
  delta$bat = subset(data, batterId == playerId)$delta.bat
  delta$br = c(subset(data, start1B == playerId)$delta.br1
               , subset(data, start2B == playerId)$delta.br2, subset(data, start3B == playerId)$delta.br3)
  delta$pitch = subset(data, pitcherId == playerId)$delta.pitch
  delta$field = c(subset(data, pitcherId == playerId)$delta.field, subset(data, playerId.C == playerId)$delta.field
               , subset(data, playerId.1B == playerId)$delta.field, subset(data, playerId.2B == playerId)$delta.field
               , subset(data, playerId.3B == playerId)$delta.field, subset(data, playerId.SS == playerId)$delta.field
               , subset(data, playerId.LF == playerId)$delta.field, subset(data, playerId.CF == playerId)$delta.field
             , subset(data, playerId.RF == playerId)$delta.field)
  raa = sapply(delta, bstrap)
  ns = unlist(sapply(raa, nrow))
  out = data.frame(playerId = playerId, component = rep(names(ns), ns), raa = do.call("rbind", raa))
  return(out)
}

bstrap = function (x, N = 5000) {
  if (length(x) > 0) {
    return(do(N) * sum(resample(x)))
  } else {
    return(NULL)
  }
}

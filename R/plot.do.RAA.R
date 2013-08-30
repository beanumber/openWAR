#' @title plot.do.RAA
#' 
#' @description Visualize WAR
#' 
#' @details Density Plot for WAR estimates
#' 
#' @param playerIds A vector of valid MLBAM player IDs present in the data argument
#' @param data A data.frame resulting from shakeWAR() of class \code{do.RAA}
#' 
#' @return a faceted densityplot
#' 
#' @export
#' @examples
#' 
#' ds = getData()
#' # not run
#' openWAR = makeWAR(ds)
#' openWAR.sim = shakeWAR(openWAR)
#' plot(data=openWAR.sim, playerIds = c(431151, 502517, 408234, 285078, 518774, 285079))

plot.do.RAA = function (data, playerIds = c(431151, 285079), ...) {
  require(mosaic)
  playerIds = sort(playerIds)
  # is it worth the trouble to filter the rows? 
  rows = subset(data, batterId %in% playerIds)
  # Remove unused factor levels
  rows$Name = factor(rows$Name)
  
  lkup = unique(rows[,c("batterId", "Name")])
  labels = as.character(lkup[order(lkup$batterId),]$Name)
  
  sims.long = reshape(rows[,c("batterId", "Name", "RAA", "RAA.bat", "RAA.br", "RAA.field", "RAA.pitch")], varying = 3:7
          , timevar = "component", direction = "long")
  
  plot = densityplot(~RAA | component, groups=batterId, data=sims.long
                     , panel = function(x,y,...) {
                       panel.densityplot(x, plot.points=FALSE, lwd = 3,...)
                     }
                     , auto.key=list(columns=min(4, length(playerIds)), text = labels)
                     , ylim = c(-0.01, 0.2)
                     , xlab = "Runs Above Average (RAA)"
  )
  return(plot)
}










# Deprecated

# warplot = function (playerIds, data, N = 5000, ...) {
#   require(mosaic)
#   bgcol = "darkgray"
#   playerIds = sort(playerIds)
#   # is it worth the trouble to filter the rows? 
#   #  rows = subset(data, batterId %in% playerIds | start1B %in% playerIds | start2B %in% playerIds | start3B %in% )
#   # Remove unused factor levels
# #  data$batterName = factor(rows$batterName)
#   lkup = unique(data[,c("batterId", "batterName")])
#   lkup = subset(lkup, batterId %in% playerIds)
#   labels = as.character(lkup[order(lkup$batterId),]$batterName)
#   bstraps = lapply(playerIds, getWARest, data = data, N = N)
#   X = do.call("rbind", bstraps)
#   X = merge(x=X, y=lkup, by.x="playerId", by.y="batterId")
#   
#   plot = densityplot(~result | component, groups=playerId, data=X
#          , panel = function(x,y,...) {
#            panel.densityplot(x, plot.points=FALSE, lwd = 3, ...)
#            if (length(bstraps) == 1) {
#              quants = qdata(c(0.05, 0.5, 0.975), vals=x)
#              h = 0.025
#              eps = 0.05 * diff(range(x))
#              panel.abline(v=quants[1], col=bgcol)
#              panel.abline(v=quants[2], col=bgcol)
#              panel.abline(v=quants[3], col=bgcol)
#              panel.text(quants[2], h, paste("Point Estimate:\n", round(quants[2],2)))
#              panel.text(quants[1], h/3, paste(round(quants[1],2)))
#              panel.text(quants[3], h/3, paste(round(quants[3],2)))
#              panel.arrows(quants[1] + eps, h/3, quants[3] - eps, h/3, ends = "both")
#              panel.text(quants[2], h/2, "95% CI")
#            }
#          }
#        , auto.key=list(columns=min(4, length(playerIds)), text = labels)
#        , ylim = c(-0.01, 0.2)
#        , xlab = "Runs Above Average (RAA)"
#   )
#   return(plot)
# }

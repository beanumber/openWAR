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

warplot = function (batterIds, data, N = 5000, ...) {
  require(mosaic)
  bgcol = "darkgray"
  batterIds = sort(batterIds)
  xs = subset(data, batterId %in% batterIds)
  # Remove unused factor levels
  xs$batterName = factor(xs$batterName)
  lkup = aggregate(batterName ~ batterId, data = xs[,c("batterId", "batterName")], unique)
  bstraps = sapply(batterIds, bstrap, data = xs, N = N)
  
  X = data.frame(batterId = rep(batterIds, each=N), name = rep(lkup$batterName, each=N), raa = do.call("c", bstraps))
  if (length(bstraps) == 1) {
    quants = qdata(c(0.05, 0.5, 0.975), vals=bstraps$result)
    h = 0.025
    eps = 0.05 * diff(range(bstraps$result))
  }
  plot = densityplot(~raa, groups=name, data=X
         , panel = function(x,y,...) {
           panel.densityplot(x, plot.points=FALSE, ...)
           if (length(bstraps) == 1) {
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
       , auto.key=list(columns=min(4, length(batterIds)))
       , xlab = "Runs Above Average (RAA)"
  )
  return(plot)
}

bstrap = function (batter, data, N = 5000, ...) {
  xs = subset(data, batterId == batter)$delta.bat
  bstrap = do(N) * sum(resample(xs), na.rm=TRUE)
  return(bstrap)
}

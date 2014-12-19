#' @title openWARPlayers
#' 
#' @description A data.frame of players and their tabulated openWAR values.  The function \code{getWAR} returns an object of class \code{openWARPlayers}
#' 
#' @exportClass openWARPlayers
#' @examples showClass("openWARPlayers")

setClass("openWARPlayers", contains = "data.frame")

#' @title summary.openWARPlayers
#' 
#' @description Summarize WAR among players
#' 
#' @details A summary of players' WAR
#' 
#' @param data An object of class \code{"openWARPlayers"}
#' 
#' @export summary.openWARPlayers
#' @examples
#' 
#' ds <- getData()
#' out <-  (makeWAR(ds))
#' war <- getWAR(out$openWARPlays)
#' summary(war)

summary.openWARPlayers = function (data, n = 25, ...) {  
  cat(paste("Displaying information for", nrow(data), "players, of whom", nrow(filter(data, RAA.pitch != 0)), "have pitched\n"))
  
  # classic syntax
  # head(data[order(data$WAR, decreasing=TRUE), c("Name", "TPA", "WAR", "RAA", "repl", "RAA.bat", "RAA.br", "RAA.field", "RAA.pitch")], n)
  
  # dplyr syntax
  data %>%
    dplyr::select(Name, TPA, WAR, RAA, repl, RAA.bat, RAA.br, RAA.field, RAA.pitch) %>%
    arrange(desc(WAR)) %>%
    head(n)
}




#' @title plot.openWARPlayers
#' 
#' @description Display a season's worth of openWAR results
#' 
#' @details Given an openWARPlayers object, draw a plot displaying each player's RAA, WAR, and replacement
#' level shadow. 
#' 
#' @param data An object of class \code{"openWARPlayers"}
#' 
#' @export plot.openWARPlayers
#' 
#' @examples
#' 
#' ds = getData()
#' out = makeWAR(ds)
#' players = getWAR(out$openWAR)
#' summary(players)
#' plot(players)

plot.openWARPlayers = function (data, ...) {
  # Add the combined playing time
  data = transform(data, TPA = PA.bat + BF)
  
  supp = data[,c("playerId", "Name", "WAR", "TPA", "repl", "RAA", "RAA.pitch")]
  names(supp) = c("playerId", "Name", "WAR", "TPA", "repl", "RAA", "RAA_pitch")
  
  require(mosaic)
  p = xyplot(RAA ~ TPA, groups=isReplacement, data=data, panel=panel.war, data2 = supp
         , alpha = 0.3, pch = 19, type = c("p", "r")
         , par.settings = list("superpose.symbol" = list(pch = 19))
         , ylab = "openWAR Runs Above Average", xlab = "Playing Time (plate appearances plus batters faced)"
         , auto.key = list(columns = 2, corner = c(0.05,0.95), text = c("MLB Player", "Replacement Player"))
         , sub = paste("Number of Players =", nrow(data), ", Number of Replacement Level Players =", sum(data$isReplacement))
         , ...
  )
  print(p)
}

#' @title panel.war
#' 
#' @description Display a season's worth of openWAR results
#' 
#' @details Given an openWARPlayers object, draw a plot displaying each player's RAA, WAR, and replacement
#' level shadow. 
#' 
#' @param x
#' @param y
#' @param ... arguments passed from \code{"plot.openWARPlayers"}
#' 
#' @export panel.war
#' 
#' @examples
#' 
#' ds = getData()
#' out = makeWAR(ds)
#' players = getWAR(out$openWAR)
#' summary(players)
#' plot(players)

panel.war = function (x, y, ...) {
  panel.abline(h=0, col = "black")
  panel.xyplot(x, y, ...)            
  # data2 is passed to the panel function via the ellipses,
  # so extract those arguments vial match.call
  args <- match.call(expand.dots = FALSE)$...
  ds = args$data2
  panel.xyplot(ds$TPA, ds$repl, col="darkgray", ...)
  # annotate the best player
  best.idx = which.max(ds$WAR)
  with(ds[best.idx,], panel.arrows(TPA, repl, TPA, RAA, code=3, lwd=2, col="darkgray", length=0.1))
  with(ds[best.idx,], panel.text(TPA, RAA, Name, pos = 4))
  # annotate the best pitcher
  pitchers = subset(ds, RAA_pitch > 0)
  pitcher.idx = which.max(pitchers$WAR)
  with(pitchers[pitcher.idx,], panel.arrows(TPA, repl, TPA, RAA, code=3, lwd=2, col="darkgray", length=0.1))
  with(pitchers[pitcher.idx,], panel.text(TPA, RAA, Name, pos = 3))
  # annotate the worst player
  worst.idx = which.min(ds$WAR)
  with(ds[worst.idx,], panel.arrows(TPA, repl, TPA, RAA, code=3, lwd=2, col="darkgray", length=0.1))
  with(ds[worst.idx,], panel.text(TPA, RAA, Name, pos = 2))
  # annotate the total WAR in the system
  panel.text(0, ds[best.idx, "RAA"] * 0.6, paste("Total RAA =", round(sum(y), 1)), adj=0)
  panel.text(0, ds[best.idx, "RAA"] * 0.6 - 3, paste("Total WAR =", round(sum(ds$WAR), 1)), adj=0)
}


##############################################################
#
# Generic functions for bootstrapped results
#
#########################################################


#' @title summary.do.openWARPlayers
#' 
#' @description Summarize WAR
#' 
#' @details Summary of players' WAR
#' 
#' @param An object of class \code{"openWARPlayers"}
#' 
#' @export
#' 
#' @examples
#' 
#' ds = makeWAR(ds)
#' sim = shakeWAR(ds)
#' summary(sim)

summary.do.openWARPlayers = function (data, n = 25, ...) {
#   require(plyr)  
#   players = ddply(data, ~Name, summarise, q0 = min(WAR), q2.5 = quantile(WAR, 0.025)
#                   , q25 = quantile(WAR, 0.25)
#                   , q50 = mean(WAR)
#                   , q75 = quantile(WAR, 0.75), q97.5 = quantile(WAR, 0.975), q100 = max(WAR))
#   print(head(players[order(players$q50, decreasing=TRUE),], n))
  
  data %>%
    dplyr::select(Name, WAR) %>%
    group_by(Name) %>%
    summarise(q0 = min(WAR)
              , q2.5 = quantile(WAR, 0.025)
              , q25 = quantile(WAR, 0.25)
              , q50 = mean(WAR)
              , q75 = quantile(WAR, 0.75)
              , q97.5 = quantile(WAR, 0.975)
              , q100 = max(WAR)) %>%
    arrange(desc(q50)) %>%
    head(n)
}




#' @title plot.do.openWARPlayers
#' 
#' @description Visualize WAR
#' 
#' @details Density Plot for WAR estimates
#' 
#' @param playerIds A vector of valid MLBAM player IDs present in the data argument
#' @param data A data.frame resulting from shakeWAR() of class \code{do.openWARPlayers}
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

plot.do.openWARPlayers = function (data, playerIds = c(431151, 285079), ...) {
  require(mosaic)
  playerIds = sort(playerIds)
  # is it worth the trouble to filter the rows? 
  rows = filter(data, batterId %in% playerIds)
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


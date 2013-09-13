#' @title openWARPlayers
#' 
#' @description A data.frame of players and their tabulated openWAR values
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
#' @param data An object of class \code{"openWAR"}
#' 
#' @export summary.openWARPlayers
#' @examples
#' 
#' ds = getData()
#' out = (makeWAR(ds))
#' summary(out)

summary.openWARPlayers = function (data, n = 25, ...) {
  cat(paste("Displaying information for", nrow(data), "players, of whom", nrow(subset(data, RAA.pitch != 0)), "have pitched\n"))
  head(data[order(data$WAR, decreasing=TRUE), c("Name", "TPA", "WAR", "RAA", "repl", "RAA.bat", "RAA.br", "RAA.field", "RAA.pitch")], n)
}

#' @title getWAR
#' 
#' @description Tabulates WAR
#' 
#' @details Compute each player's WAR, given their RAA values
#' 
#' @param data An object of class \code{"openWARPlays"}
#' 
#' @export getWAR
#' @export getWAR.openWARPlays
#' 
#' @examples
#' 
#' ds = getData()
#' out = makeWAR(ds)
#' raa = getRAA(out$openWAR)
#' war = getWAR(raa)

getWAR = function (data, verbose=TRUE, ...) UseMethod("getWAR")

getWAR.openWARPlays = function (data, verbose=TRUE, ...) {
  # Get the replacement level players
  players = getRAA(data)
  replIds = getReplacementPlayers(players)
  repl = subset(players, batterId %in% replIds)
  
  if (verbose) {
    message(paste("...identified", nrow(repl), "replacement-level players..."))
  }
  
  # Find the playing time matrix for all players
  message(paste("...determining playing time for all", nrow(players), "players..."))
  message("...this may take some time...")
  pt.mat = getPlayingTime(data, players$batterId)
  repl.means = getReplacementMeans(data, replIds)
  repl.value = pt.mat %*% repl.means
  
  # Attach the replacement values to the list of players
  war = merge(x=players, y=data.frame(batterId = row.names(repl.value), repl = repl.value), all.x = TRUE)
  war = transform(war, WAR = (RAA - repl) / 10)
  class(war) = c("openWAR", class(war))
  return(war)
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
  require(plyr)  
  players = ddply(data, ~Name, summarise, q0 = min(RAA), q2.5 = quantile(RAA, 0.025), q50 = mean(RAA), q97.5 = quantile(RAA, 0.975), q100 = max(RAA))
  print(head(players[order(players$q50, decreasing=TRUE),], n))
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


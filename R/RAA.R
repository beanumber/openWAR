#' @title RAA
#' 
#' @description Contains the output from getWAR()
#' 
#' @exportClass RAA
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

summary.RAA = function (data, n = 25, ...) {
  cat(paste("Displaying information for", nrow(data), "players, of whom", nrow(subset(data, RAA.pitch != 0)), "have pitched\n"))
  head(data[order(data$RAA, decreasing=TRUE), c("Name", "TPA", "RAA", "RAA.bat", "RAA.br", "RAA.field", "RAA.pitch")], n)
}

#' @title getReplacementPlayers
#' 
#' @description Identifies the replacement level players
#' 
#' @details Returns a list of playerIds, who constitute replacement-level players
#' 
#' @param data An object of class \code{"RAA"}
#' 
#' @export getReplacementPlayers
#' @examples
#' 
#' ds = getData()
#' out = makeWAR(ds)
#' players = getWAR(out)
#' summary(out)
#' replacementIds = getReplacementPlayers(players)

getReplacementPlayers = function (data) {
  # Get the list of all playerIds
  playerIds = universe$batterId
  # Order by plate appearances
  universe = data[order(data$PA, decreasing=TRUE),]
  # Find the players with the most plate appearances, 13 for each club
  mlb.pos.playerIds = universe$batterId[1:(30 * 13)]
  
  universe = data[order(data$BF, decreasing=TRUE),]
  # Find the players with the most batters faced, 12 per club
  mlb.pitcherIds = universe$batterId[1:(30*12)]
  
  # Their union are the MLB players
  mlb.playerIds = union(mlb.pos.playerIds, mlb.pitcherIds)
  # Everyone else is replacement level
  repl.playerIds = setdiff(playerIds, mlb.playerIds)
  
  return(repl.playerIds)
}




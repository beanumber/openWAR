#' @title openWARPlays
#' 
#' @description Contains the output from makeWAR() specific to openWAR
#' 
#' @exportClass openWARPlays
#' @examples showClass("openWARPlays")

setClass("openWARPlays", contains = "data.frame")

#' @title getWAR
#' 
#' @description Tabulates RAA values per player
#' 
#' @details takes an openWARPlays data.frame and consolidates the RAA values by player
#' 
#' @param data An openWARPlays data.frame
#' 
#' @return an RAA object
#' 
#' @export getWAR
#' @export getWAR.openWARPlays
#' @examples
#' 
#' data = getData()
#' ds = makeWAR(data)
#' players = getWAR(ds$openWAR)
#' summary(players)
#' 

getWAR = function (data, ...) UseMethod("getWAR")

getWAR.openWARPlays = function (data, ...) {
  message("...Tabulating RAA per player...")
  require(plyr)
  war.bat = ddply(data, ~ batterId, summarise, Name = max(as.character(batterName))
                  , PA = sum(isPA), G = length(unique(gameId)), HR = sum(event=="Home Run")
                  , RAA.bat = sum(raa.bat, na.rm=TRUE))
  # war.br0 = ddply(data, ~batterId, summarise, RAA.br0 = sum(raa.br0, na.rm=TRUE))
  war.br1 = ddply(data, ~start1B, summarise, RAA.br1 = sum(raa.br1, na.rm=TRUE))
  war.br2 = ddply(data, ~start2B, summarise, RAA.br2 = sum(raa.br2, na.rm=TRUE))
  war.br3 = ddply(data, ~start3B, summarise, RAA.br3 = sum(raa.br3, na.rm=TRUE))
  
  war.P = ddply(data, ~pitcherId, summarise, RAA.P = sum(raa.P, na.rm=TRUE))
  war.C = ddply(data, ~playerId.C, summarise, RAA.C = sum(raa.C, na.rm=TRUE))
  war.1B = ddply(data, ~playerId.1B, summarise, RAA.1B = sum(raa.1B, na.rm=TRUE))
  war.2B = ddply(data, ~playerId.2B, summarise, RAA.2B = sum(raa.2B, na.rm=TRUE))
  war.3B = ddply(data, ~playerId.3B, summarise, RAA.3B = sum(raa.3B, na.rm=TRUE))
  war.SS = ddply(data, ~playerId.SS, summarise, RAA.SS = sum(raa.SS, na.rm=TRUE))
  war.LF = ddply(data, ~playerId.LF, summarise, RAA.LF = sum(raa.LF, na.rm=TRUE))
  war.CF = ddply(data, ~playerId.CF, summarise, RAA.CF = sum(raa.CF, na.rm=TRUE))
  war.RF = ddply(data, ~playerId.RF, summarise, RAA.RF = sum(raa.RF, na.rm=TRUE))
  war.pitch = ddply(data, ~ pitcherId, summarise, Name = max(as.character(pitcherName)), BF = sum(isPA), RAA.pitch = sum(raa.pitch))
  
  # players = merge(x=war.bat, y=war.br0, by.x="batterId", by.y="batterId", all=TRUE)
  players = merge(x=war.bat, y=war.br1, by.x="batterId", by.y="start1B", all=TRUE)
  players = merge(x=players, y=war.br2, by.x="batterId", by.y="start2B", all=TRUE)
  players = merge(x=players, y=war.br3, by.x="batterId", by.y="start3B", all=TRUE)
  players = merge(x=players, y=war.pitch, by.x="batterId", by.y="pitcherId", all=TRUE)
  players$Name = with(players, ifelse(is.na(Name.x), Name.y, Name.x))
  players = merge(x=players, y=war.P, by.x="batterId", by.y="pitcherId", all=TRUE)
  players = merge(x=players, y=war.C, by.x="batterId", by.y="playerId.C", all=TRUE)
  players = merge(x=players, y=war.1B, by.x="batterId", by.y="playerId.1B", all=TRUE)
  players = merge(x=players, y=war.2B, by.x="batterId", by.y="playerId.2B", all=TRUE)
  players = merge(x=players, y=war.3B, by.x="batterId", by.y="playerId.3B", all=TRUE)
  players = merge(x=players, y=war.SS, by.x="batterId", by.y="playerId.SS", all=TRUE)
  players = merge(x=players, y=war.LF, by.x="batterId", by.y="playerId.LF", all=TRUE)
  players = merge(x=players, y=war.CF, by.x="batterId", by.y="playerId.CF", all=TRUE)
  players = merge(x=players, y=war.RF, by.x="batterId", by.y="playerId.RF", all=TRUE)
  players[is.na(players)] = 0
  players = transform(players, RAA.br =  RAA.br1 + RAA.br2 + RAA.br3)
  players = transform(players, RAA.off = RAA.bat + RAA.br)
  players = transform(players, RAA.field = RAA.P + RAA.C + RAA.1B + RAA.2B + RAA.3B + RAA.SS + RAA.LF + RAA.CF + RAA.RF)
  players = transform(players, RAA = RAA.bat + RAA.br + RAA.pitch + RAA.field)
  players = transform(players, TPA = PA + BF)
  players = players[, setdiff(names(players), c("Name.x", "Name.y"))]
  class(players) = c("RAA", "data.frame")
  return(players)
}

#' @title getReplacementActivity
#' 
#' @description Isolates the plays involving replacement-level players
#' 
#' @details Returns only the RAA values involving replacement-level players
#' 
#' @param data An openWARPlays data.frame
#' @param replacementIds A vector of playerIds for replacement-level players
#' 
#' @return a list of RAA values for each type of activity
#' 
#' @export getReplacementActivity
#' @export getReplacementActivity.openWARPlays
#' @examples
#' 
#' data = getData()
#' ds = makeWAR(data)
#' players = getWAR(ds$openWAR)
#' summary(players)
#' 


getReplacementActivity = function (data, replacementIds) UseMethod("getReplacementActivity")

getReplacementActivity.openWARPlays = function (data, replacementIds) {
  out = list()
  out[["bat"]] = subset(data, batterId %in% replacementIds, select="raa.bat")
  out[["br1"]] = subset(data, start1B %in% replacementIds, select="raa.br1")
  out[["br2"]] = subset(data, start2B %in% replacementIds, select="raa.br2")
  out[["br3"]] = subset(data, start3B %in% replacementIds, select="raa.br3")
  out[["pitch"]] = subset(data, pitcherId %in% replacementIds, select="raa.pitch")
  out[["P"]] = subset(data, pitcherId %in% replacementIds, select="raa.P")
  out[["C"]] = subset(data, playerId.C %in% replacementIds, select="raa.C")
  out[["1B"]] = subset(data, playerId.1B %in% replacementIds, select="raa.1B")
  out[["2B"]] = subset(data, playerId.2B %in% replacementIds, select="raa.2B")
  out[["3B"]] = subset(data, playerId.3B %in% replacementIds, select="raa.3B")
  out[["SS"]] = subset(data, playerId.SS %in% replacementIds, select="raa.SS")
  out[["LF"]] = subset(data, playerId.LF %in% replacementIds, select="raa.LF")
  out[["CF"]] = subset(data, playerId.CF %in% replacementIds, select="raa.CF")
  out[["RF"]] = subset(data, playerId.RF %in% replacementIds, select="raa.RF")
  return(out)
}



#' @title getPlayingTime
#' @aliases getPlayingTimeOnce
#' 
#' @description Identify the playing time for all players
#' 
#' @details Returns a matrix of playing time at each activity for each player
#' 
#' @param data An openWARPlays data.frame
#' @param playerIds A vector of playerIds 
#' 
#' @return a matrix of playing time counts for each player at each position
#' 
#' @export getPlayingTime
#' @export getPlayingTime.openWARPlays
#' @examples
#' 
#' data = getData()
#' ds = makeWAR(data)
#' players = getWAR(ds$openWAR)
#' summary(players)
#' 


getPlayingTimeOnce.openWARPlays = function (data, playerId) {
  reality = data[c("batterId", "start1B", "start2B", "start3B", "pitcherId", "pitcherId", "playerId.C", "playerId.1B"
                   , "playerId.2B", "playerId.3B", "playerId.SS", "playerId.LF", "playerId.CF", "playerId.RF")]
  is.player = (reality == playerId)
  pt = apply(is.player, 2, sum, na.rm=TRUE)
  return(pt)
}

getPlayingTime = function (data, playerIds) UseMethod("getPlayingTime")

getPlayingTime.openWARPlays = function (data, playerIds) {
  out = t(sapply(playerIds, getPlayingTimeOnce.openWARPlays, data=data))
}

#' @title getReplacementMeans
#' 
#' @description Compute the mean RAA for replacement-level players
#' 
#' @details Identifies replacement-level players, and then compute their contribution
#' 
#' @param data An openWARPlays data.frame
#' 
#' @return a vector of mean contributions per activity for replacement-level players
#' 
#' @export getReplacementMeans
#' @export getReplacementMeans.openWARPlays
#' @examples
#' 
#' data = getData()
#' ds = makeWAR(data)
#' getReplacementMeans(ds$openWAR)
#' 

getReplacementMeans = function (data) UseMethod("getReplacementMeans")

getReplacementMeans.openWARPlays = function (data) {
  replIds = getReplacementPlayers(getWAR(data))
  repl = getReplacementActivity(data, replIds)
  repl.means = sapply(repl, mean, na.rm=TRUE)
  return(repl.means)
}



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

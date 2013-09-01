#' @title GameDayPlaysExt
#' 
#' @description Contains the output from makeWAR()
#' 
#' @exportClass GameDayPlaysExt
#' @examples showClass("GameDayPlaysExt")

setClass("GameDayPlaysExt", contains = "GameDayPlays")




#' @title shakeWAR
#' 
#' @description resample a data.frame to obtain variance estimate for WAR
#' 
#' @details Resamples the rows of an MLBAM data set
#' 
#' @param data An MLBAM data.frame 
#' @param N the number of resamples (default 5000)
#' 
#' @return a data.frame with RAA values 
#' 
#' @export
#' @examples
#' 
#' ds = getData()
#' ds = makeWAR()
#' res = shakeWAR(ds)
#' 
#' 

shakeWAR = function (data, N = 10, ...) UseMethod("shakeWAR")

shakeWAR.GameDayPlaysExt = function (data, N = 10, ...) {
  require(mosaic)
  raa.fields = c("raa.bat", "raa.br1", "raa.br2", "raa.br3", "raa.pitch", "raa.P", "raa.C", "raa.1B"
                 , "raa.2B", "raa.3B", "raa.SS", "raa.LF", "raa.CF", "raa.RF")
  if (length(intersect(raa.fields, names(data))) < length(raa.fields)) {
    message("...missing some RAA fields...making WAR again...")
    ds = makeWAR(data)
  } else {
    ds = data
  }
  id.fields = c("batterId", "start1B", "start2B", "start3B", "pitcherId", "playerId.C", "playerId.1B"
                , "playerId.2B", "playerId.3B", "playerId.SS", "playerId.LF", "playerId.CF", "playerId.RF"
                , "batterName", "pitcherName", "gameId", "event")
  ds.raa = ds[, c(id.fields, raa.fields)]
  bstrap = do(N) * getWAR(resample(ds.raa))
  return(bstrap)
}

#' @title getWAR
#' 
#' @description Retrieve openWAR
#' 
#' @details Retrieves openWAR, aggregated by player, given an MLBAM data set
#' 
#' @param data An MLBAM data.frame 
#' 
#' @return a data.frame of RAA values per player
#' 
#' @export getWAR.GameDayPlaysExt
#' @examples
#' 
#' ds = getData()
#' res = getWAR(ds)
#' 


getWAR = function (data, recompute = FALSE, ...) UseMethod("getWAR")

getWAR.GameDayPlaysExt = function (data, recompute = FALSE, ...) {
  # Check to see if the WAR fields already exist
  raa.fields = c("raa.bat", "raa.br1", "raa.br2", "raa.br3", "raa.pitch", "raa.P", "raa.C", "raa.1B"
                 , "raa.2B", "raa.3B", "raa.SS", "raa.LF", "raa.CF", "raa.RF")
  if (length(intersect(raa.fields, names(data))) < length(raa.fields) | recompute) {
    ds = makeWAR(data)
  } else {
    ds = data
  }
  # Work only with the columns of the data that we need
  ds = ds[, c("batterId", "batterName", "pitcherId", "pitcherName"
              , "start1B", "start2B", "start3B"
              , "playerId.C", "playerId.1B", "playerId.2B", "playerId.3B"
              , "playerId.SS", "playerId.LF", "playerId.CF", "playerId.RF"
              , "gameId", "isPA", "event", raa.fields)]
  
  message("...Tabulating RAA per player...")
  require(plyr)
  war.bat = ddply(ds, ~ batterId, summarise, Name = max(as.character(batterName))
                  , PA = sum(isPA), G = length(unique(gameId)), HR = sum(event=="Home Run")
                  , RAA.bat = sum(raa.bat, na.rm=TRUE))
  # war.br0 = ddply(ds, ~batterId, summarise, RAA.br0 = sum(raa.br0, na.rm=TRUE))
  war.br1 = ddply(ds, ~start1B, summarise, RAA.br1 = sum(raa.br1, na.rm=TRUE))
  war.br2 = ddply(ds, ~start2B, summarise, RAA.br2 = sum(raa.br2, na.rm=TRUE))
  war.br3 = ddply(ds, ~start3B, summarise, RAA.br3 = sum(raa.br3, na.rm=TRUE))
  
  war.P = ddply(ds, ~pitcherId, summarise, RAA.P = sum(raa.P, na.rm=TRUE))
  war.C = ddply(ds, ~playerId.C, summarise, RAA.C = sum(raa.C, na.rm=TRUE))
  war.1B = ddply(ds, ~playerId.1B, summarise, RAA.1B = sum(raa.1B, na.rm=TRUE))
  war.2B = ddply(ds, ~playerId.2B, summarise, RAA.2B = sum(raa.2B, na.rm=TRUE))
  war.3B = ddply(ds, ~playerId.3B, summarise, RAA.3B = sum(raa.3B, na.rm=TRUE))
  war.SS = ddply(ds, ~playerId.SS, summarise, RAA.SS = sum(raa.SS, na.rm=TRUE))
  war.LF = ddply(ds, ~playerId.LF, summarise, RAA.LF = sum(raa.LF, na.rm=TRUE))
  war.CF = ddply(ds, ~playerId.CF, summarise, RAA.CF = sum(raa.CF, na.rm=TRUE))
  war.RF = ddply(ds, ~playerId.RF, summarise, RAA.RF = sum(raa.RF, na.rm=TRUE))
  war.pitch = ddply(ds, ~ pitcherId, summarise, Name = max(as.character(pitcherName)), BF = sum(isPA), RAA.pitch = sum(raa.pitch))
  
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

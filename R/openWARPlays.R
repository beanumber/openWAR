#' @title openWARPlays
#' 
#' @description Contains the output from makeWAR() specific to openWAR
#' 
#' @exportClass openWARPlays
#' @examples showClass("openWARPlays")

setClass("openWARPlays", contains = "data.frame")

#' @title getRAA
#' 
#' @description Tabulates RAA values per player
#' 
#' @details takes an openWARPlays data.frame and consolidates the RAA values by player
#' 
#' @param data An openWARPlays data.frame
#' 
#' @return an RAA object
#' 
#' @export getRAA
#' @export getRAA.openWARPlays
#' @examples
#' 
#' data = getData()
#' ds = makeWAR(data)
#' players = getRAA(ds$openWAR)
#' summary(players)
#' 

getRAA = function (data, ...) UseMethod("getRAA")

getRAA.openWARPlays = function (data, ...) {
  message("...Tabulating RAA per player...")
  
  # Batting
#  require(plyr)
#   war.bat = ddply(data, ~ batterId, summarise, playerId = batterId[1], Name = max(as.character(batterName))
#                   , PA.bat = sum(isPA), G = length(unique(gameId)), HR = sum(event=="Home Run")
#                   , RAA.bat = sum(raa.bat, na.rm=TRUE))

  # dplyr syntax
  war.bat <- data %>%
    group_by(batterId) %>%
    summarise(playerId = batterId[1]
              , Name = max(as.character(batterName))
              , PA.bat = sum(isPA)
              , G = length(unique(gameId))
              , HR = sum(event=="Home Run")
              , RAA.bat = sum(raa.bat, na.rm=TRUE))

  # Baserunning
  # war.br0 = ddply(data, ~batterId, summarise, RAA.br0 = sum(raa.br0, na.rm=TRUE))  
#   war.br1 = ddply(subset(data, !is.na(start1B)), ~start1B, summarise, PA.br1 = sum(isPA), RAA.br1 = sum(raa.br1, na.rm=TRUE))
#   war.br2 = ddply(subset(data, !is.na(start2B)), ~start2B, summarise, PA.br2 = sum(isPA), RAA.br2 = sum(raa.br2, na.rm=TRUE))
#   war.br3 = ddply(subset(data, !is.na(start3B)), ~start3B, summarise, PA.br3 = sum(isPA), RAA.br3 = sum(raa.br3, na.rm=TRUE))

  # dplyr
  war.br1 <- data %>%
    filter(!is.na(start1B)) %>%
    group_by(start1B) %>%
    summarise(PA.br1 = sum(isPA), RAA.br1 = sum(raa.br1, na.rm=TRUE))

  war.br2 <- data %>%
    filter(!is.na(start2B)) %>%
    group_by(start2B) %>%
    summarise(PA.br2 = sum(isPA), RAA.br2 = sum(raa.br2, na.rm=TRUE))

  war.br3 <- data %>%
    filter(!is.na(start3B)) %>%
    group_by(start3B) %>%
    summarise(PA.br3 = sum(isPA), RAA.br3 = sum(raa.br3, na.rm=TRUE))


  # Fielding
#   war.P = ddply(data, ~pitcherId, summarise, PA.P = sum(isPA), RAA.P = sum(raa.P, na.rm=TRUE))
#   war.C = ddply(data, ~playerId.C, summarise, PA.C = sum(isPA), RAA.C = sum(raa.C, na.rm=TRUE))
#   war.1B = ddply(data, ~playerId.1B, summarise, PA.1B = sum(isPA), RAA.1B = sum(raa.1B, na.rm=TRUE))
#   war.2B = ddply(data, ~playerId.2B, summarise, PA.2B = sum(isPA), RAA.2B = sum(raa.2B, na.rm=TRUE))
#   war.3B = ddply(data, ~playerId.3B, summarise, PA.3B = sum(isPA), RAA.3B = sum(raa.3B, na.rm=TRUE))
#   war.SS = ddply(data, ~playerId.SS, summarise, PA.SS = sum(isPA), RAA.SS = sum(raa.SS, na.rm=TRUE))
#   war.LF = ddply(data, ~playerId.LF, summarise, PA.LF = sum(isPA), RAA.LF = sum(raa.LF, na.rm=TRUE))
#   war.CF = ddply(data, ~playerId.CF, summarise, PA.CF = sum(isPA), RAA.CF = sum(raa.CF, na.rm=TRUE))
#   war.RF = ddply(data, ~playerId.RF, summarise, PA.RF = sum(isPA), RAA.RF = sum(raa.RF, na.rm=TRUE))

  war.P <- data %>%
    group_by(pitcherId) %>%
    summarise(PA.P = sum(isPA), RAA.P = sum(raa.P, na.rm=TRUE))
  war.C <- data %>%
    group_by(playerId.C) %>%
    summarise(PA.C = sum(isPA), RAA.C = sum(raa.C, na.rm=TRUE))
  war.1B <- data %>%
    group_by(playerId.1B) %>%
    summarise(PA.1B = sum(isPA), RAA.1B = sum(raa.1B, na.rm=TRUE))
  war.2B <- data %>%
    group_by(playerId.2B) %>%
    summarise(PA.2B = sum(isPA), RAA.2B = sum(raa.2B, na.rm=TRUE))  
  war.3B <- data %>%
    group_by(playerId.3B) %>%
    summarise(PA.3B = sum(isPA), RAA.3B = sum(raa.3B, na.rm=TRUE))
  war.SS <- data %>%
    group_by(playerId.SS) %>%
    summarise(PA.SS = sum(isPA), RAA.SS = sum(raa.SS, na.rm=TRUE))  
  war.LF <- data %>%
    group_by(playerId.LF) %>%
    summarise(PA.LF = sum(isPA), RAA.LF = sum(raa.LF, na.rm=TRUE))
  war.CF <- data %>%
    group_by(playerId.CF) %>%
    summarise(PA.CF = sum(isPA), RAA.CF = sum(raa.CF, na.rm=TRUE))
  war.RF <- data %>%
    group_by(playerId.RF) %>%
    summarise(PA.RF = sum(isPA), RAA.RF = sum(raa.RF, na.rm=TRUE))


  # Pitching

#  war.pitch = ddply(data, ~ pitcherId, summarise, Name = max(as.character(pitcherName)), BF = sum(isPA), RAA.pitch = sum(raa.pitch))

  war.pitch <- data %>%
    group_by(pitcherId) %>%
    summarise(Name = max(as.character(pitcherName)), BF = sum(isPA), RAA.pitch = sum(raa.pitch))
  
  # Merge them all together

  # players = merge(x=war.bat, y=war.br0, by.x="batterId", by.y="batterId", all=TRUE)
  players = merge(x=war.bat, y=war.br1, by.x="playerId", by.y="start1B", all=TRUE)
  players = merge(x=players, y=war.br2, by.x="playerId", by.y="start2B", all=TRUE)
  players = merge(x=players, y=war.br3, by.x="playerId", by.y="start3B", all=TRUE)
  players = merge(x=players, y=war.pitch, by.x="playerId", by.y="pitcherId", all=TRUE)
  players$Name = with(players, ifelse(is.na(Name.x), Name.y, Name.x))
  players = merge(x=players, y=war.P, by.x="playerId", by.y="pitcherId", all=TRUE)
  players = merge(x=players, y=war.C, by.x="playerId", by.y="playerId.C", all=TRUE)
  players = merge(x=players, y=war.1B, by.x="playerId", by.y="playerId.1B", all=TRUE)
  players = merge(x=players, y=war.2B, by.x="playerId", by.y="playerId.2B", all=TRUE)
  players = merge(x=players, y=war.3B, by.x="playerId", by.y="playerId.3B", all=TRUE)
  players = merge(x=players, y=war.SS, by.x="playerId", by.y="playerId.SS", all=TRUE)
  players = merge(x=players, y=war.LF, by.x="playerId", by.y="playerId.LF", all=TRUE)
  players = merge(x=players, y=war.CF, by.x="playerId", by.y="playerId.CF", all=TRUE)
  players = merge(x=players, y=war.RF, by.x="playerId", by.y="playerId.RF", all=TRUE)

  # clean up
  players[is.na(players)] = 0
  players = mutate(players, RAA.br =  RAA.br1 + RAA.br2 + RAA.br3)
  players = mutate(players, RAA.off = RAA.bat + RAA.br)
  players = mutate(players, RAA.field = RAA.P + RAA.C + RAA.1B + RAA.2B + RAA.3B + RAA.SS + RAA.LF + RAA.CF + RAA.RF)
  players = mutate(players, RAA = RAA.bat + RAA.br + RAA.pitch + RAA.field)
  players = mutate(players, TPA = PA.bat + BF)
  players = players[, setdiff(names(players), c("Name.x", "Name.y"))]
  class(players) = c("openWARPlayers", "data.frame")
  return(players)
}

#' @title getWAR
#' 
#' @description Tabulates WAR
#' 
#' @details Compute each player's WAR, given their RAA values
#' 
#' @param data An object of class \code{"openWARPlays"}
#' @param nteams The nteams argument to be passed to getReplacementPlayers
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

getWAR = function (data, nteams = 30, verbose=TRUE, ...) UseMethod("getWAR")

getWAR.openWARPlays = function (data, nteams = 30, verbose=TRUE, ...) {
  # Get the replacement level players
  players = getRAA(data)
  replIds = getReplacementPlayers(players, nteams)
  repl = filter(players, playerId %in% replIds)
  
  if (verbose) {
    message(paste("...identified", nrow(repl), "replacement-level players..."))
  }
  
  # Find the playing time matrix for all players
  pt.mat = as.matrix(players[, c("PA.bat", "PA.br1", "PA.br2", "PA.br3", "BF", "PA.P", "PA.C", "PA.1B"
                       , "PA.2B", "PA.3B", "PA.SS", "PA.LF", "PA.CF", "PA.RF")])
  
  repl.means = getReplacementMeans(data, replIds)
  players$repl = as.numeric(pt.mat %*% repl.means)
  
  # Attach the replacement values to the list of players
#  war = merge(x=players, y=data.frame(batterId = row.names(repl.value), repl = repl.value), all.x = TRUE)
  war <- mutate(players, WAR = (RAA - repl) / 10)
  war <- mutate(war, isReplacement = playerId %in% replIds)
  class(war) = c("openWARPlayers", class(war))
  return(war)
}

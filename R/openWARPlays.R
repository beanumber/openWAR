#' @title openWARPlays
#' 
#' @description Contains the output from the function \code{\link{makeWAR}}
#' specific to \code{openWAR}.
#' 
#' @param x an object
#' @examples 
#' class(MayProcessed$openWAR)

openWARPlays <- function(x) { 
  class(x) <- c("openWARPlays", class(x))
}

#' @title getRAA
#' 
#' @description Tabulates runs above average (RAA) values per player
#' 
#' @details Takes an object of class \code{\link{openWARPlays}} and consolidates the RAA values by player
#' 
#' @param data An object of class \code{\link{openWARPlays}} 
#' @param ... currently ignored
#' 
#' @return A data.frame of class \code{\link{openWARPlayers}}
#' 
#' @import dplyr
#' @export

getRAA = function(data, ...) UseMethod("getRAA")

#' @rdname getRAA
#' @method getRAA openWARPlays
#' @export

getRAA.openWARPlays = function(data, ...) {
    message("...Tabulating RAA per player...")
        
    # dplyr syntax
    war.bat <- data %>% 
      group_by_(~batterId) %>% 
      summarise_(playerId = ~batterId[1], Name = ~max(as.character(batterName)), PA.bat = ~sum(isPA), 
        G = ~length(unique(gameId)), HR = ~sum(event == "Home Run"), RAA.bat = ~sum(raa.bat, na.rm = TRUE))
    
    # Baserunning war.br0 = ddply(data, ~batterId, summarise, RAA.br0 = sum(raa.br0, na.rm=TRUE)) war.br1 = ddply(subset(data,
    # !is.na(start1B)), ~start1B, summarise, PA.br1 = sum(isPA), RAA.br1 = sum(raa.br1, na.rm=TRUE)) war.br2 =
    # ddply(subset(data, !is.na(start2B)), ~start2B, summarise, PA.br2 = sum(isPA), RAA.br2 = sum(raa.br2, na.rm=TRUE)) war.br3
    # = ddply(subset(data, !is.na(start3B)), ~start3B, summarise, PA.br3 = sum(isPA), RAA.br3 = sum(raa.br3, na.rm=TRUE))
    
    # dplyr
    war.br1 <- data %>% 
    	dplyr::filter_(~!is.na(start1B)) %>% 
    	group_by_(~start1B) %>% 
    	summarise_(PA.br1 = ~sum(isPA), RAA.br1 = ~sum(raa.br1, na.rm = TRUE))
    
    war.br2 <- data %>% 
    	dplyr::filter_(~!is.na(start2B)) %>% 
    	group_by_(~start2B) %>% 
    	summarise_(PA.br2 = ~sum(isPA), RAA.br2 = ~sum(raa.br2, na.rm = TRUE))
    
    war.br3 <- data %>% 
    	dplyr::filter_(~!is.na(start3B)) %>% 
    	group_by_(~start3B) %>% 
    	summarise_(PA.br3 = ~sum(isPA), RAA.br3 = ~sum(raa.br3, na.rm = TRUE))
    
    
    # Fielding war.P = ddply(data, ~pitcherId, summarise, PA.P = sum(isPA), RAA.P = sum(raa.P, na.rm=TRUE)) war.C = ddply(data,
    # ~playerId.C, summarise, PA.C = sum(isPA), RAA.C = sum(raa.C, na.rm=TRUE)) war.1B = ddply(data, ~playerId.1B, summarise,
    # PA.1B = sum(isPA), RAA.1B = sum(raa.1B, na.rm=TRUE)) war.2B = ddply(data, ~playerId.2B, summarise, PA.2B = sum(isPA),
    # RAA.2B = sum(raa.2B, na.rm=TRUE)) war.3B = ddply(data, ~playerId.3B, summarise, PA.3B = sum(isPA), RAA.3B = sum(raa.3B,
    # na.rm=TRUE)) war.SS = ddply(data, ~playerId.SS, summarise, PA.SS = sum(isPA), RAA.SS = sum(raa.SS, na.rm=TRUE)) war.LF =
    # ddply(data, ~playerId.LF, summarise, PA.LF = sum(isPA), RAA.LF = sum(raa.LF, na.rm=TRUE)) war.CF = ddply(data,
    # ~playerId.CF, summarise, PA.CF = sum(isPA), RAA.CF = sum(raa.CF, na.rm=TRUE)) war.RF = ddply(data, ~playerId.RF,
    # summarise, PA.RF = sum(isPA), RAA.RF = sum(raa.RF, na.rm=TRUE))
    
    war.P <- data %>% 
    	group_by_(~pitcherId) %>% 
    	summarise_(PA.P = ~sum(isPA), RAA.P = ~sum(raa.P, na.rm = TRUE))
    war.C <- data %>% 
    	group_by_(~playerId.C) %>% 
    	summarise_(PA.C = ~sum(isPA), RAA.C = ~sum(raa.C, na.rm = TRUE))
    war.1B <- data %>% 
    	group_by_(~playerId.1B) %>% 
    	summarise_(PA.1B = ~sum(isPA), RAA.1B = ~sum(raa.1B, na.rm = TRUE))
    war.2B <- data %>% 
    	group_by_(~playerId.2B) %>% 
    	summarise_(PA.2B = ~sum(isPA), RAA.2B = ~sum(raa.2B, na.rm = TRUE))
    war.3B <- data %>% 
    	group_by_(~playerId.3B) %>% 
    	summarise_(PA.3B = ~sum(isPA), RAA.3B = ~sum(raa.3B, na.rm = TRUE))
    war.SS <- data %>% 
    	group_by_(~playerId.SS) %>% 
    	summarise_(PA.SS = ~sum(isPA), RAA.SS = ~sum(raa.SS, na.rm = TRUE))
    war.LF <- data %>% 
    	group_by_(~playerId.LF) %>% 
    	summarise_(PA.LF = ~sum(isPA), RAA.LF = ~sum(raa.LF, na.rm = TRUE))
    war.CF <- data %>% 
    	group_by_(~playerId.CF) %>% 
    	summarise_(PA.CF = ~sum(isPA), RAA.CF = ~sum(raa.CF, na.rm = TRUE))
    war.RF <- data %>% 
    	group_by_(~playerId.RF) %>% 
    	summarise_(PA.RF = ~sum(isPA), RAA.RF = ~sum(raa.RF, na.rm = TRUE))
    
    
    # Pitching
    
    # war.pitch = ddply(data, ~ pitcherId, summarise, Name = max(as.character(pitcherName)), BF = sum(isPA), RAA.pitch =
    # sum(raa.pitch))
    
    war.pitch <- data %>% 
    	group_by_(~pitcherId) %>% 
    	summarise_(Name = ~max(as.character(pitcherName)), BF = ~sum(isPA), RAA.pitch = ~sum(raa.pitch))
    
    # Merge them all together
    
    # players = merge(x=war.bat, y=war.br0, by.x='batterId', by.y='batterId', all=TRUE)
    players = merge(x = war.bat, y = war.br1, by.x = "playerId", by.y = "start1B", all = TRUE)
    players = merge(x = players, y = war.br2, by.x = "playerId", by.y = "start2B", all = TRUE)
    players = merge(x = players, y = war.br3, by.x = "playerId", by.y = "start3B", all = TRUE)
    players = merge(x = players, y = war.pitch, by.x = "playerId", by.y = "pitcherId", all = TRUE)
    players$Name = with(players, ifelse(is.na(Name.x), Name.y, Name.x))
    players = merge(x = players, y = war.P, by.x = "playerId", by.y = "pitcherId", all = TRUE)
    players = merge(x = players, y = war.C, by.x = "playerId", by.y = "playerId.C", all = TRUE)
    players = merge(x = players, y = war.1B, by.x = "playerId", by.y = "playerId.1B", all = TRUE)
    players = merge(x = players, y = war.2B, by.x = "playerId", by.y = "playerId.2B", all = TRUE)
    players = merge(x = players, y = war.3B, by.x = "playerId", by.y = "playerId.3B", all = TRUE)
    players = merge(x = players, y = war.SS, by.x = "playerId", by.y = "playerId.SS", all = TRUE)
    players = merge(x = players, y = war.LF, by.x = "playerId", by.y = "playerId.LF", all = TRUE)
    players = merge(x = players, y = war.CF, by.x = "playerId", by.y = "playerId.CF", all = TRUE)
    players = merge(x = players, y = war.RF, by.x = "playerId", by.y = "playerId.RF", all = TRUE)
    
    # clean up
    players[is.na(players)] = 0
    players = mutate_(players, RAA.br = ~(RAA.br1 + RAA.br2 + RAA.br3))
    players = mutate_(players, RAA.off = ~(RAA.bat + RAA.br))
    players = mutate_(players, RAA.field = ~(RAA.P + RAA.C + RAA.1B + RAA.2B + RAA.3B + RAA.SS + RAA.LF + RAA.CF + RAA.RF))
    players = mutate_(players, RAA = ~(RAA.bat + RAA.br + RAA.pitch + RAA.field))
    players = mutate_(players, TPA = ~(PA.bat + BF))
    players = players[, setdiff(names(players), c("Name.x", "Name.y", "batterId"))]
    class(players) = c("openWARPlayers", class(players))
    return(players)
} 

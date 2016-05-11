#' @title Determines the replacement players.
#' @aliases getReplacementPlayers.RAA
#' @aliases getReplacementPlayers.openWARPlays
#' 
#' @description  This function determines and returns the group of players who 
#' are considered to be replacement-level players.  
#' 
#' @details In the openWAR framework, we first define a specific number of 
#' players to be considered 'true' major league talent.  This function returns 
#' the group of players who do not appear in the group of 'true' major league talent.
#' The performances of the players in this group are used to estimate the value 
#' of a replacement-level player for use in converting runs above average (RAA) 
#' into runs above replacement (RAR) and ultimately wins above replacement (WAR).  
#' 
#' @param data An object of class \code{\link{openWARPlays}}
#' @param nteams the number of teams used to calculate the number of non-replacement
#' players. The default is 30 since that is the number of MLB teams. (Note: Using 27.5
#' seems to result in a total WAR that is close to 1000.)
#' @param ... currently ignored
#' 
#' @note This function is used internally in the function \code{getWAR}
#' 
#' @export getReplacementPlayers
#' 
#' @examples
#' 
#' 
#' players = getWAR(MayProcessed$openWAR)
#' summary(players)
#' replacementIds = getReplacementPlayers(players)

getReplacementPlayers = function(data, nteams = 30, ...) UseMethod("getReplacementPlayers")

#' @export
#' @importFrom utils head
#' @rdname getReplacementPlayers
#' @method getReplacementPlayers openWARPlayers

getReplacementPlayers.openWARPlayers = function(data, nteams = 30, ...) {
    # Get the list of all playerIds
    playerIds = as.numeric(data$playerId)
    
    # Order by plate appearances 
    # Find the players with the most plate appearances, 13 for each club
    mlb.pos.playerIds <- data %>%
      arrange_(~desc(PA.bat)) %>%
      select_(~playerId) %>%
      utils::head(round(nteams * 13))

    # Find the players with the most batters faced, 12 per club
    mlb.pitcherIds <- data %>%
      arrange_(~desc(BF)) %>%
      select_(~playerId) %>%
      head(round(nteams * 12))
    
    # Their union are the MLB players
    mlb.playerIds = as.numeric(union(mlb.pos.playerIds$playerId, mlb.pitcherIds$playerId))
    # Everyone else is replacement-level
    repl.playerIds = setdiff(playerIds, as.numeric(mlb.playerIds))
    
    return(repl.playerIds)
}

#' @export
#' @rdname getReplacementPlayers
#' @method getReplacementPlayers openWARPlays
#' 
getReplacementPlayers.openWARPlays = function(data, nteams = 30, ...) {
    players = getWAR(data)
    return(getReplacementPlayers.openWARPlayers(players, nteams = 30, ...))
}

#' @title getReplacementActivity
#' 
#' @description Isolates the plays involving replacement-level players
#' 
#' @details Returns only the RAA values involving replacement-level players
#' 
#' @param data A data.frame of class \code{\link{openWARPlays}}
#' @param replacementIds A vector of playerIds for replacement-level players
#' 
#' @return A list of RAA values for each type of activity that generates RAA values
#' 
#' @note This function is used internally in the function \code{getWAR}
#' 
#' @export getReplacementActivity
#' @examples
#' 
#' players = getWAR(MayProcessed$openWAR)
#' summary(players)
#' replIds = getReplacementPlayers(players)
#' replacementActivity = getReplacementActivity(MayProcessed$openWAR,replIds)
#' 


getReplacementActivity = function(data, replacementIds) UseMethod("getReplacementActivity")


#' @export
#' @rdname getReplacementActivity
#' @method getReplacementActivity openWARPlays

getReplacementActivity.openWARPlays = function(data, replacementIds) {
    out = list()
    out[["bat"]] = dplyr::filter_(data, ~batterId %in% replacementIds)$raa.bat
    out[["br1"]] = dplyr::filter_(data, ~start1B %in% replacementIds)$raa.br1
    out[["br2"]] = dplyr::filter_(data, ~start2B %in% replacementIds)$raa.br2
    out[["br3"]] = dplyr::filter_(data, ~start3B %in% replacementIds)$raa.br3
    out[["pitch"]] = dplyr::filter_(data, ~pitcherId %in% replacementIds)$raa.pitch
    out[["P"]] = dplyr::filter_(data, ~pitcherId %in% replacementIds)$raa.P
    out[["C"]] = dplyr::filter_(data, ~playerId.C %in% replacementIds)$raa.C
    out[["1B"]] = dplyr::filter_(data, ~playerId.1B %in% replacementIds)$raa.1B
    out[["2B"]] = dplyr::filter_(data, ~playerId.2B %in% replacementIds)$raa.2B
    out[["3B"]] = dplyr::filter_(data, ~playerId.3B %in% replacementIds)$raa.3B
    out[["SS"]] = dplyr::filter_(data, ~playerId.SS %in% replacementIds)$raa.SS
    out[["LF"]] = dplyr::filter_(data, ~playerId.LF %in% replacementIds)$raa.LF
    out[["CF"]] = dplyr::filter_(data, ~playerId.CF %in% replacementIds)$raa.CF
    out[["RF"]] = dplyr::filter_(data, ~playerId.RF %in% replacementIds)$raa.RF
    return(out)
}



#' @title getReplacementMeans
#' 
#' @description Compute the mean RAA for replacement-level players
#' 
#' @details Identifies replacement level players and then computes their mean contribution
#' 
#' @param data A data.frame of class \code{\link{openWARPlays}}
#' @param replIds MLBAM IDs of specific players that you want to designate as replacement-level
#' 
#' @return A numeric vector of mean contributions per activity for replacement-level players
#' 
#' @export getReplacementMeans
#' @examples
#' 
#' 
#' players = getWAR(MayProcessed$openWAR)
#' summary(players)
#' replIds = getReplacementPlayers(players)
#' replMeans = getReplacementMeans(MayProcessed$openWAR, replIds)
#' 

getReplacementMeans = function(data, replIds) UseMethod("getReplacementMeans")

#' @export
#' @rdname getReplacementMeans
#' @method getReplacementMeans openWARPlays

getReplacementMeans.openWARPlays = function(data, replIds) {
    repl = getReplacementActivity(data, replIds)
    repl.means = sapply(repl, mean, na.rm = TRUE)
    return(repl.means)
} 

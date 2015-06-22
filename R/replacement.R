#' @title getReplacementPlayers
#' @aliases getReplacementPlayers.RAA
#' @aliases getReplacementPlayers.openWARPlays
#' 
#' @description  This function determines and returns a list of players who are considered to be replacement level players.  
#' 
#' @details In the openWAR framework, we first define a specific number of players to be considered 'true' major league talent.  This function returns a list of players who do not appear on the list of 'true' major league talent.
#' The performances of the players on this list are used to estimate the value of a replacement level player for use in converting runs above average into runs above replacement and ultimately wins above replacement.  
#' 
#' @param data An object of class \code{'RAA'}
#' @param nteams the number of teams used to calculate the number of non-replacement
#' players. The default is 30 since that is the number of MLB teams. Using 27.5
#' seems to result in a total WAR that is close to 1000. 
#' @param ... currently ignored
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
#' @rdname getReplacementPlayers
#' @method getReplacementPlayers openWARPlayers

getReplacementPlayers.openWARPlayers = function(data, nteams = 30, ...) {
    # Get the list of all playerIds
    playerIds = data$playerId
    # Order by plate appearances universe = data[order(data$PA.bat, decreasing=TRUE),]
    universe <- arrange_(data, ~desc(PA.bat))
    # Find the players with the most plate appearances, 13 for each club
    mlb.pos.playerIds = universe$batterId[1:round(nteams * 13)]
    
    # universe = data[order(data$BF, decreasing=TRUE),]
    universe <- arrange_(data, ~desc(BF))
    # Find the players with the most batters faced, 12 per club
    mlb.pitcherIds = universe$playerId[1:round(nteams * 12)]
    
    # Their union are the MLB players
    mlb.playerIds = union(mlb.pos.playerIds, mlb.pitcherIds)
    # Everyone else is replacement level
    repl.playerIds = setdiff(playerIds, mlb.playerIds)
    
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
#' @param data An openWARPlays data.frame
#' @param replacementIds A vector of playerIds for replacement-level players
#' 
#' @return a list of RAA values for each type of activity
#' 
#' @export getReplacementActivity
#' @examples
#' 
#' players = getWAR(MayProcessed$openWAR)
#' summary(players)
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
#' @details Identifies replacement-level players, and then compute their contribution
#' 
#' @param data An openWARPlays data.frame
#' @param replIds MLBAM IDs of specific players that you want to designate as replacement level
#' 
#' @return a vector of mean contributions per activity for replacement-level players
#' 
#' @export getReplacementMeans
#' @examples
#' 
#' 
#' raa <- getRAA(MayProcessed$openWAR)
#' replIds = getReplacementPlayers(raa, 30)
#' getReplacementMeans(MayProcessed$openWAR, replIds)
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

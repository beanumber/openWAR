#' @title getWAR
#' 
#' @description Tabulates WAR
#' 
#' @details Compute each player's WAR, given their RAA values
#' 
#' @param data An object of class \code{'openWARPlays'}
#' @param dataRepl An object of class \code{'openWARPlays'} that will be used to calculate the replacement level.  
#' @param nteams The nteams argument to be passed to getReplacementPlayers
#' @param verbose do you want information about how many replacement-level players there are?
#' @param ... currently ignored
#' 
#' @return An data.frame of class \code{openWARPlayers}
#' 
#' @export getWAR
#' @export getWAR.openWARPlays
#' @export getWAR.list
#' @export getWAR.GameDayPlays
#' 
#' @examples
#' 
#' \dontrun{
#' MayProcessed = makeWAR(May)
#' }
#' war = getWAR(MayProcessed)
#' war = getWAR(MayProcessed, nteams = 27.5)

getWAR = function(data, dataRepl = NULL, nteams = 30, verbose = TRUE, ...) UseMethod("getWAR")

#' @rdname getWAR
#' @export
#' 
#' @examples 
#' \dontrun{
#' war = getWAR(May)  
#' }

getWAR.GameDayPlays = function(data, dataRepl = NULL, nteams = 30, verbose = TRUE, ...) {
  madeWAR <- makeWAR(data)
  return(getWAR.list(madeWAR$openWAR, dataRepl, nteams, verbose, ...))
}

#' @rdname getWAR
#' @export getWAR.list
#' 
#' @examples
#' 
#' \dontrun{
#' MayProcessed = makeWAR(May)
#' }
#' war = getWAR(MayProcessed)  

getWAR.list = function(data, dataRepl = NULL, nteams = 30, verbose = TRUE, ...) {
  return(getWAR.openWARPlays(data$openWAR, dataRepl, nteams, verbose, ...))
}

#' @rdname getWAR
#' @export getWAR.openWARPlays
#' 
#' @examples
#' 
#' \dontrun{
#' MayProcessed = makeWAR(May)
#' }
#' war <- getWAR(MayProcessed$openWAR)
#' war <- getWAR(MayProcessed$openWAR, dataRepl = head(MayProcessed$openWAR, 10000))
#' 

getWAR.openWARPlays = function(data, dataRepl = NULL, nteams = 30, verbose = TRUE, ...) {
  
    # Compute RAA for all players
    players <- getRAA(data)
    # If dataRepl is provided, calculate replacement level based on this data.  
    if (!is.null(dataRepl) & "openWARPlays" %in% class(dataRepl)) {
      playersRepl = getRAA(dataRepl)
    }  else {
    # If dataRepl is not provided, then use all the data to define replacement level.
    # getRAA will only be run once.  
      dataRepl <- data
      playersRepl <- players
    }
    
    replIds = getReplacementPlayers(playersRepl, nteams)
    repl = playersRepl[playersRepl$playerId %in% replIds, ]
    
    if (verbose) {
        message(paste("...identified", nrow(repl), "replacement-level players..."))
    }
    
    # Find the playing time matrix for all players
    pt.mat = as.matrix(players[, c("PA.bat", "PA.br1", "PA.br2", "PA.br3", "BF", "PA.P", "PA.C", "PA.1B", "PA.2B", "PA.3B", 
        "PA.SS", "PA.LF", "PA.CF", "PA.RF")])
    
    repl.means = getReplacementMeans(dataRepl, replIds)
    players$repl = as.numeric(pt.mat %*% repl.means)
    
    # Attach the replacement values to the list of players war = merge(x=players, y=data.frame(batterId = row.names(repl.value),
    # repl = repl.value), all.x = TRUE)
    war <- mutate_(players, WAR = ~((RAA - repl)/10))
    war <- mutate_(war, isReplacement = ~playerId %in% replIds)
    class(war) = c("openWARPlayers", class(war))
    return(war)
} 

#' 
#' @title Compute each player's openWAR.
#' 
#' @description Tabulates each player's value of openWAR given either an object of class \code{GameDayPlays} 
#' or a list containing a data frame of class \code{openWARPlays}.
#' 
#' @details If an object of \code{GameDayPlays} is input to the function \code{getWAR}, the function \code{makeWAR} is called internally to first process 
#' the data, then \code{getWAR} tabulates the final values of openWAR for each player.  Alternatively, the function 
#' \code{makeWAR} can be run separately and the output from this function can be passed to \code{getWAR}, which will complete the tabulation of openWAR.  
#' By default, the data set used to compute the replacement level is the same as the input data set.  Using the \code{dataRepl} option
#' a user can specify a separate data set of used solely to calculate the replacement level.  This data set should be based on a full season of data 
#' and not a partial season.  For in season, calculations, for instance, the data from the current year can be used to calculate openWAR, whereas 
#' the data from the previous full season can be used to compute the replacement level.  
#' 
#' 
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
#' 
#' @examples
#' 
#' \dontrun{
#' MayProcessed = makeWAR(May)
#' war = getWAR(MayProcessed)
#' }
#' #openWAR and replacement level calculated with the same data set
#' war = getWAR(MayProcessed)
#' #openWAR and replacement level calculated with the same data set
#' #Replacement level calculated as if there were only 27.5 major league teams
#' war = getWAR(MayProcessed, nteams = 27.5)

getWAR = function(data, dataRepl = NULL, nteams = 30, verbose = TRUE, ...) UseMethod("getWAR")

#' @rdname getWAR
#' @export
#' @method getWAR GameDayPlays
#' 
#' @examples 
#' \dontrun{
#' #openWAR and replacement level calculated with the same data set
#' war = getWAR(MayProcessed)
#' }

getWAR.GameDayPlays = function(data, dataRepl = NULL, nteams = 30, verbose = TRUE, ...) {
  madeWAR <- makeWAR(data)
  return(getWAR(madeWAR$openWAR, dataRepl, nteams, verbose, ...))
}

#' @rdname getWAR
#' @export
#' @method getWAR list
#' 
#' @examples
#' 
#' \dontrun{
#' MayProcessed = makeWAR(May)
#' }
#' #openWAR and replacement level calculated with the same data set
#' war = getWAR(MayProcessed)  

getWAR.list = function(data, dataRepl = NULL, nteams = 30, verbose = TRUE, ...) {
  return(getWAR(data$openWAR, dataRepl, nteams, verbose, ...))
}

#' @rdname getWAR
#' @export
#' @method getWAR openWARPlays
#' 
#' @examples
#' 
#' \dontrun{
#' MayProcessed = makeWAR(May)
#' }
#' #openWAR and replacement level calculated with the same data set
#' war <- getWAR(MayProcessed$openWAR)
#' #Use different data for calculating openWAR and replacement level  
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

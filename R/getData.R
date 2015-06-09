#' @title getData
#' 
#' @description Retrieves MLBAM GameDay files for a specified time interval using multiple calls of the gameday function.  
#' 
#' @details Given a beginning and end date, this function will retrieve all data from the MLABM
#' GameDay server in the specified interval and process them into a single data.frame. 
#' 
#' @param start A valid date in yyyy-mm-dd format (default yesterday)
#' @param end A valid date in yyyy-mm-dd format (default start)
#' @param drop.suspended Logical indicating whether games with fewer than 5 innings should be excluded.  Default is TRUE.
#' 
#' @return A data.frame of class 'GameDayPlays' consisting of play-by-play data 
#' 
#' @import dplyr
#'
#' @export
#' @examples
#' 
#' # Get data from one day
#' ds = getData(start = '2013-03-31')
#' # Get data from multiple days
#' \dontrun{
#' ds = getData(start = '2013-03-31', end = '2013-04-02')
#' }
#' # Get data for entire 2013 season
#' \dontrun{
#' ds = getData(start = '2013-03-31', end = '2013-09-30')
#' }

getData <- function(start = Sys.Date() - 1, end = NULL, gameIds = NULL, drop.suspended = TRUE) {
    if (is.null(gameIds)) {
        if (is.null(end)) {
            end = start
        }
        dates = seq(from = as.Date(start), to = as.Date(end), by = "1 day")
        gIds = unlist(sapply(dates, getGameIds))
    } else {
        gIds = gameIds
    }

    # if (require(multicore)) { message('Using multicore to parallelize!') gd.list = mclapply(gIds,
    # gameday, mc.cores = max(1, getOption('core') - 1)) } else {
    gd.list = lapply(gIds, gameday)
    # }
    ds.list = lapply(gd.list, "[[", "ds")
    out = do.call(rbind, ds.list)
    out = dplyr::filter_(out, ~game_type == "R")
    # exclude suspended games
    if (drop.suspended) {
        # test = ddply(out, ~gameId, summarise, Innings = max(inning))
        test <- dplyr::summarise_(group_by_(out, ~gameId), Innings = ~max(inning))
        suspended = dplyr::filter_(test, ~Innings < 5)$gameId
        out = dplyr::filter_(out, ~!gameId %in% suspended)
    }
    
    # Set the class attribute
    class(out) <- c("GameDayPlays", "data.frame")
    return(out)
}


#' @title getDataMonthly
#' 
#' @description Retrieves MLBAM GameDay files for a single month 
#' 
#' @details Given a year and month, this function will retrieve data from the 
#' GameDay server from the specified month and process them into a single data.frame.
#' 
#' @param yyyy A year
#' @param m a numeric value corresponding to a month
#' 
#' @return A data.frame of class GameDayPlays consisting of play-by-play data 
#' 
#' @export
#' @export getDataMonthly
#' @examples
#' 
#' #Retrieve all of the MLBAM data from May 2013
#' \dontrun{
#' # ds = getDataMonthly(2013, 5)
#' }

getDataMonthly <- function(yyyy = 2013, m = 5) {
    start = as.Date(paste(yyyy, m, "01", sep = "-"), "%Y-%m-%d")
    end = as.Date(paste(yyyy, m + 1, "01", sep = "-"), "%Y-%m-%d") - 1
    return(getData(start, end))
}

#' @title getDataWeekly
#' 
#' @description Retrieves MLBAM GameDay files for a single week
#' 
#' @details Given a date, this function will retrieve data from the week starting on the specified date from the 
#' GameDay server and process them into a single data.frame.
#' 
#' @param start A valid date in yyyy-mm-dd format (default Sys.Date()-8)
#' 
#' @return A data.frame of class 'GameDayPlays' consisting of play-by-play data 
#' 
#' @export
#' @export getDataWeekly
#' @examples
#' 
#' #Retrieve all the data from the first week of the 2013 season
#' \dontrun{
#' ds = getDataWeekly('2013-03-31')
#' }


getDataWeekly <- function(start = Sys.Date() - 8) {
    return(getData(as.Date(start), as.Date(start) + 6))
}


#' @title getGameIds
#' 
#' @description Retrieves MLBAM gameIds for a specified date
#' 
#' @details Downloads information for a given day from the MLBAM website and retrieves
#' a list of valid gameIds. This function is used internally in the function getData.
#' 
#' @param date A date in 'yyyy-mm-dd' format
#'  
#' @return A vector of gameIds
#' 
#' @export
#' @examples
#' getGameIds()
#' getGameIds('2008-05-14')
#' 

getGameIds <- function(date = Sys.Date()) {
    if (class(as.Date(date)) != "Date") {
        warning("Not a valid Date")
    }
    # coerce string into a valid Date
    date = as.Date(date)
    yyyy = format(date, "%Y")
    mm = format(date, "%m")
    dd = format(date, "%d")
    url <- paste("http://gd2.mlb.com/components/game/mlb/year_", yyyy, "/month_", mm, "/day_", dd, "/", sep = "")
    cat(paste("\nRetrieving data from", date, "..."))
    a <- RCurl::getURL(url)
    b <- strsplit(a, "<a")
    ind <- grep("gid", b[[1]])
    games <- substring(b[[1]][ind], 8, 37)
    cat(paste("\n...found", length(games), "games\n"))
    return(games)
}

#' @title updateGame
#' 
#' @description Replaces data from a single game
#' 
#' @details Deletes, and then rbinds fresh information from a particular game.
#' 
#' @param gameId A valid MLBAM gameId
#' @param data a data.frame returned by getData()
#'  
#' @return a data.frame
#' 
#' @export
#' @examples
#' 
#' data(May)
#' newMay <- updateGame('gid_2013_05_14_kcamlb_anamlb_1', data = May)
#' 

updateGame <- function(gameId.vec, data, ...) {
    temp = dplyr::filter_(data, ~!gameId %in% gameId.vec)
    ds.new = getData(gameIds = unique(gameId.vec))
    out = rbind(temp, ds.new)
    return(out)
}

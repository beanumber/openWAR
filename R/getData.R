#' @title Retrieves MLBAM GameDay files
#' 
#' @description Retrieves MLBAM GameDay files for a specified time interval using
#'  multiple calls of the \code{\link{gameday}} function.  
#' 
#' @details Given a beginning and end date, this function will retrieve all data from the MLBAM
#' GameDay server in the specified interval and process them into a single data frame. 
#' 
#' @param start A valid date in yyyy-mm-dd format (default is \code{\link{Sys.Date}() - 1} (i.e. yesterday))
#' @param end A valid date in yyyy-mm-dd format (default is \code{NULL}, which then uses the same value as \code{start} (i.e. yesterday))
#' @param gameIds a vector of specific gameIds that you want to retrieve. If 
#' NULL (the default), then the dates are used to fetch the relevant gameIds
#' @param drop.suspended Logical indicating whether games with fewer than 5 innings should be excluded.  Default is \code{TRUE}.
#' 
#' @return A \code{\link{tbl_df}} of class \code{\link{GameDayPlays}} consisting of play-by-play data 
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
    gd.list <- lapply(gIds, gameday)
    # }
    ds.list <- lapply(gd.list, "[[", "ds")
    out <- dplyr::bind_rows(ds.list)
    out <- dplyr::filter_(out, ~game_type == "R")
    # exclude suspended games
    if (drop.suspended) {
        # test = ddply(out, ~gameId, summarise, Innings = max(inning))
        test <- dplyr::summarise_(group_by_(out, ~gameId), Innings = ~max(inning))
        suspended <- dplyr::filter_(test, ~Innings < 5)$gameId
        out <- dplyr::filter_(out, ~!gameId %in% suspended)
    }
    
    # Set the class attribute
    class(out) <- c("GameDayPlays", class(out))
    return(out)
}


#' @rdname getData
#' @param yyyy A year
#' @param m A numeric value corresponding to a month
#' @export
#' @examples
#' 
#' # Retrieve all of the MLBAM data from May 2013
#' \dontrun{
#' # ds = getDataMonthly(2013, 5)
#' }

getDataMonthly <- function(yyyy = 2013, m = 5) {
    start = as.Date(paste(yyyy, m, "01", sep = "-"), "%Y-%m-%d")
    end = as.Date(paste(yyyy, m + 1, "01", sep = "-"), "%Y-%m-%d") - 1
    return(getData(start, end))
}

#' @rdname getData
#' @export
#' @examples
#' 
#' # Retrieve all the data from the first week of the 2013 season
#' \dontrun{
#' ds = getDataWeekly('2013-03-31')
#' }


getDataWeekly <- function(start = Sys.Date() - 8) {
    return(getData(as.Date(start), as.Date(start) + 6))
}


#' @title Retrives gameIds.
#' 
#' @description Retrieves MLBAM gameIds for a specified date.
#' 
#' @details Downloads information for a given day from the MLBAM website and retrieves
#' a list of valid gameIds. This function is used internally in the function \code{getData}.
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
    url <- paste("http://gd2.mlb.com/components/game/mlb/year_", yyyy, "/month_", mm, "/day_", dd, sep = "")
    message(paste("\nRetrieving data from", date, "..."))
    a <- RCurl::getURL(url)
    b <- strsplit(a, "<a")
    ind <- grep("gid", b[[1]])
    games <- substring(b[[1]][ind], 15, 44) #The location of the gid moved in the URL.
    message(paste("\n...found", length(games), "games\n"))
    return(games)
}

#' Update data for a single game from GameDay
#' 
#' @description Replaces data from a single game.
#' 
#' @details Deletes and then appends fresh information from a particular game to the original data.
#' 
#' @param gameIds A character vector of valid MLBAM gameIds
#' @param data a data.frame returned by \code{\link{getData}}
#' @param ... currently ignored
#'  
#' @return A data.frame of class \code{\link{GameDayPlays}} consisting of play-by-play data 
#' 
#' @export
#' @examples
#' 
#' data(May)
#' dim(May)
#' tail(May)
#' newMay <- updateGame(gameIds = 'gid_2013_05_14_kcamlb_anamlb_1', data = May)
#' dim(newMay)
#' # the replaced games are now at the bottom of the data frame
#' tail(newMay)

updateGame <- function(gameIds, data, ...) {
    temp = dplyr::filter_(data, ~!gameId %in% gameIds)
    ds.new = getData(gameIds = unique(gameIds))
    out = rbind(temp, ds.new)
    return(out)
}

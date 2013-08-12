#' @title getData
#' 
#' @description Retrieves MLBAM GameDay files for a specified time interval
#' 
#' @details Given a beginning and end date, this function will retrieve data from the MLABM
#' GameDay server, and process them into a single data.frame. 
#' 
#' @param start A valid date in yyyy-mm-dd format (default today)
#' @param end A valid date in yyyy-mm-dd format (default start)
#' @param drop.suspended Logical indicating whether games with fewer than 5 innings should be excluded
#' 
#' @return a data.frame consisting of play-by-play data 
#' 
#' @export
#' @examples
#' 
#' # Get data from yesterday
#' ds = getData()
#' # Get data from the beginning of the year
#' ds = getData(start = "2013-03-31", end = Sys.Date())
#' # Get data over an arbitrary time interval
#' ds = getData(start = "2013-05-21", end = "2013-05-31")

getData <- function(start = Sys.Date()-1, end = NULL, gameIds = NULL, drop.suspended = TRUE) {
  if (is.null(gameIds)) {
    if(is.null(end)) {
      end = start
    }
    dates = seq(from=as.Date(start), to=as.Date(end), by="1 day")
    gIds = unlist(sapply(dates, getGameIds))
  } else {
    gIds = gameIds
  }
  #test<-getGameDay(gIds[1])
  #if (require(multicore)) {
   # message("Using multicore to parallelize!")
    #gd.list = mclapply(gIds, gameday, mc.cores = max(1, getOption("core") - 1))
  #} else {
    gd.list = lapply(gIds, gameday)
  #}
  ds.list = lapply(gd.list, "[[", "ds")
  out = do.call(rbind, ds.list)
  out = subset(out, game_type == "R")
  # exclude suspended games
  if (drop.suspended) {
    test = ddply(out, ~gameId, summarise, Innings = max(inning))
    suspended = subset(test, Innings < 5)$gameId
    out = subset(out, !gameId %in% suspended)
  }
  

  
  return(out)
}


#' @title getDataMonthly
#' @aliases getDataWeekly
#' 
#' @description Retrieves MLBAM GameDay files for a single month (or week)
#' 
#' @details Given a year and month, this function will retrieve data from the 
#' GameDay server, and process them into a single data.frame.
#' 
#' @param yyyy A year
#' @param m a numeric value corresponding to a month
#' 
#' @return a data.frame consisting of play-by-play data 
#' 
#' @export
#' @export getDataWeekly
#' @examples
#' 
#' ds = getData()
#' getMonthlyData(2-13, 5)

getDataMonthly <- function(yyyy = 2013, m = 5) {
  start = as.Date(paste(yyyy, m, "01", sep="-"), "%Y-%m-%d")
  end = as.Date(paste(yyyy, m + 1, "01", sep="-"), "%Y-%m-%d") - 1
  return(getData(start, end))
}

getDataWeekly <- function(start = Sys.Date() - 8) {
  return(getData(as.Date(start), as.Date(start) + 6))
}


#' @title getGameIds
#' 
#' @description Retrieves MLBAM gameIds for a specified date
#' 
#' @details Downloads information for a given day from the MLBAM website and retrieves
#' a list of valid gameIds. 
#' 
#' @param date A date in "yyyy-mm-dd" format
#'  
#' @return a vector of gameIds
#' 
#' @export
#' @examples
#' getGameIds()
#' getGameIds("2008-05-14")
#' 

getGameIds <- function(date = Sys.Date()) {
  require(RCurl)
  if (class(as.Date(date)) != "Date") {
    warning("Not a valid Date")
  }
  yyyy = format(date, "%Y")
  mm = format(date, "%m")
  dd = format(date, "%d")
  url<-paste("http://gd2.mlb.com/components/game/mlb/year_",yyyy,"/month_",mm,"/day_",dd,"/",sep="")
  cat(paste("\nRetrieving data from", date, "..."))
  a <- getURL(url)
  b <- strsplit(a,"<a")
  ind <- grep("gid", b[[1]])
  games <- substring(b[[1]][ind], 8, 37)
  cat(paste("\n...found", length(games), "games"))
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
#' getData(start = "2013-03-31", end = "2013-07-14")
#' ds = updateGame("gid_2013_04_16_nynmlb_colmlb_2")
#' 

updateGame <- function(gameId.vec, data, ...) {
  temp = subset(data, !gameId %in% gameId.vec)
  ds.new = getData(gameIds = unique(gameId.vec))
  out = rbind(temp, ds.new)
  return(out)
}



  
########################################################################
#
# Deprecated
#
#################################################################
# 
# getGameDayURLs = function (gameId = "gid_2012_08_12_atlmlb_nynmlb_1") {
#   if (nchar(gameId) != 30) {
#     stop("This is not a valid MLBAM gameId!")
#   }
#   yyyy <- substring(gameId, 5, 8)
#   mm <- substring(gameId, 10, 11)
#   dd <- substring(gameId, 13, 14)
#   
#   # Base URL
#   base = paste("http://gd2.mlb.com/components/game/mlb/year_",yyyy,"/month_",mm,"/day_",dd,"/",sep="")
#   message(base)
#   
#   url = NULL
#   url["bis_boxscore.xml"] = paste(base, gameId, "/bis_boxscore.xml", sep="")  
#   url["inning_all.xml"] = paste(base, gameId, "/inning/inning_all.xml", sep="")
#   url["inning_hit.xml"] = paste(base, gameId, "/inning/inning_hit.xml", sep="")  
#   url["game.xml"] = paste(base, gameId, "/game.xml", sep="")  
#   url["game_events.xml"] = paste(base, gameId, "/game_events.xml", sep="")  
#   
#   
#   return(url)
# }
# 
# getGameDayXML = function (gameId = "gid_2012_08_12_atlmlb_nynmlb_1", type = "inning_all") {
#   url = getGameDayURLs(gameId)
#   
#   # If the local directory for this game does not exist, create it and download the files
#   dirname = file.path("data", gameId)
#   if (!file.exists(dirname)) {
#     warning("...GameDay XML files are not in local directory -- must download")
#     dir.create(dirname)  
#     files = getURL(url)
#     for(i in 1:(length(files))) {
#       filename = basename(names(files)[i])
#       write(files[i], file.path(dirname, filename))
#     }
#   }
#   
#   # Now read from the local files
#   xml = switch(type
#                , bis_boxscore.xml = try(xmlParse(file.path(dirname, "bis_boxscore.xml")))
#                , inning_all.xml = try(xmlTreeParse(file.path(dirname, "inning_all.xml")))
#                , game.xml = try(xmlParse(file.path(dirname, "game.xml")))
#                , game_events.xml = try(xmlTreeParse(file.path(dirname, "game_events.xml")))
#                , inning_hit.xml = try(xmlTreeParse(file.path(dirname, "inning_hit.xml")))
#   )
# #   if (class(xml) == "try-error") {
# #     warning("404 - GameDay files do not exist...")
# #     return(NULL)
# #   }
#   return(xml)
# }
# 

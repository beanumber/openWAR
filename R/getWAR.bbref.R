#' @title getWAR.bbref
#' 
#' @description Retrieves WAR from a database table
#' 
#' @details This function will retrieve bWAR data for all players within a 
#' specified timeframe. 
#' 
#' @param con An RMySQL database connection
#' @param ids A character vector of player ids that will be retrieved
#' @param start A numeric representing the start year of the interval for selecting WAR
#' @param end A numeric representing the end year of the interval for selecting WAR
#' 
#' @return A data.frame with the following elements:
#' \item{PlayerID }{Character string containnig the player id}
#' \item{yearID }{Year}
#' \item{stintId }{???}
#' \item{teamID }{The team that the player played on}
#' \item{lgId }{The league the player played in (National or American)}
#' \item{R_BAT }{Batting component}
#' \item{R_BR }{Running component}
#' \item{R_DP }{Double Play component}
#' \item{R_FIELD }{Fielding component}
#' \item{R_POS }{Position component}
#' \item{R_REPL }{Replacemnt player value}
#' \item{RAA }{Runs above average}
#' \item{RAR }{Runs above replacement}
#' \item{WAR }{Wins above replacement}
#' 
#' @export
#' @examples
#' # Create a connection to the database
#' con = getCon()
#' # Pull bWAR for 2012
#' war = getWAR(con)
#' # Pull bWAR for the 1980s
#' war = getWAR(con, start=1980, end=1989)
#' 
getWAR.bbref <- function (con, ids = NULL, start=2012, end=2012, group=FALSE) {
  playerId.default = c("wrighda03", "reyesjo01")
  
  # Make sure the list of ids is valid
  if (!is.null(ids) & typeof(ids) == "character" & length(ids) > 0) {
    pid.string = paste("AND playerId IN ('", paste(ids, collapse="','"), "')", sep="")
  } else {
    pid.string = ""    
  }
  
  # Make sure the start and end are valid
  if (!is.null(start) & start > 1870 & start < 2013) {
  } else { start = 2012 }
  if (!is.null(end) & end > 1870 & end < 2013) {
  } else { end = 2012 }
  
  if (group) {
    q = paste("SELECT playerId, yearId, sum(WAR) as WAR
      FROM war_bbref
      WHERE yearId >= ", start, " AND yearId <= ", end, 
      pid.string, "GROUP BY playerId, yearId")
  } else {
    q = paste("SELECT *
      FROM war_bbref
      WHERE yearId >= ", start, " AND yearId <= ", end, 
              pid.string)   
  }
    #  cat(q)
  res = dbGetQuery(con, q)
  return(res)
}

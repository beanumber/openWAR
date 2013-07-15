#' @title reliability
#' 
#' @description Find the reliability (autocorrelation) of WAR in consecutive
#'  seasons
#' 
#' @details The reliability of a statistic is defined as the year-to-year
#' correlation (autocorrelation) from consecutive time elements. This is simply
#' Pearson correlation coefficient between consecutive observations of the 
#' statistic for the same player. Here we find the correlation between a player's
#' WAR in year $x$ with his WAR in year $x+1$.
#' 
#' @param con An RMySQL database connection
#' @param start A numeric representing the start year of the interval for selecting WAR
#' @param end A numeric representing the end year of the interval for selecting WAR
#' 
#' @return A named vector, each entry of which represents the correlation 
#' between WAR in that year and the previous year.
#' 
#' @export
#' @examples
#' # Create a connection to the database
#' con = getCon()
#' # Examine the reliability of WAR
#' rel = reliability(con, 1980)
#' xyplot(rel ~ as.numeric(names(rel)), type=c("r", "l"))
#' 
reliability <- function (con, start=2011, end=2012) {
  war = getWAR(con, start=start, end=end, group=TRUE)
  n = length(unique(war$yearId))
  cat("\nComputing reliability for", n, " consecutive seasons...\n")
  wars = split(war, war$yearId)
  cors = sapply(1:(length(wars)-1), reliabilityHelp, war.list=wars)
  years = start:(end-1)
#  names(cors) = paste(years, years+1, sep="-")
  names(cors) = years
  return(cors)
}

reliabilityHelp <- function (war.list, idx) {
  merged = merge(x=war.list[[idx]], y=war.list[[idx+1]], by="playerId")
  return(with(merged, cor(WAR.x, WAR.y)))
}
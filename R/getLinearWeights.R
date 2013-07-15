#' @title getLinearWeights
#' 
#' @description Computes Linear Weights estimates
#' 
#' @details Pull data over a certain time period, and run MLR model to find Linear
#' Weights estimates
#' 
#' @param con An RMySQL database connection
#' @param start A numeric representing the start year of the interval for selecting WAR
#' @param end A numeric representing the end year of the interval for selecting WAR
#' 
#' @return A vector of weights
#' \item{1B }{Singles}
#' \item{2B }{Doubles}
#' \item{3B }{Triples}
#' \item{HR }{Home Runs}
#' \item{BB }{Walks}
#' \item{HBP }{Hit By Pitch}
#' \item{SO }{Strikeouts}
#' \item{GO }{Ground outs}
#' \item{AO }{Air outs}
#' \item{LO }{Line outs}
#' \item{UO }{Pop-up outs}
#' \item{Other }{Everything else -- should be 0}
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
getLinearWeights <- function (con, start=1980, end=2012) {
  this.year = format(Sys.Date(), "%Y")
  # Make sure the start and end are valid
  if (!is.null(start) & start > 1870 & start < this.year) {
  } else { start = 2012 }
  if (!is.null(end) & end > 1870 & end < this.year) {
  } else { end = 2012 }
  
  q = paste("SELECT yearId, teamId
    , RS, TPA, H - 2B - 3B - HR as X1B, 2B as X2B, 3B as X3B, HR, BB, HBP, SO, GO, AO, LO, UO
    , TPA - H - BB - HBP - SO - GO - AO - LO - UO as Other
    FROM retro_team_seasons
    WHERE yearId >= ", start, " AND yearId <= ", end)   
  
#  cat(q)
  ds = dbGetQuery(con, q)
  fm = lm(RS ~ 0 + X1B + X2B + X3B + HR + BB + HBP + SO + GO + AO + LO + UO, data=ds)
  summary(fm)
  return(coef(fm))
}

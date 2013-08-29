#' @title crosscheck.MLBAMData
#' 
#' @description Cross-check the accuracy of the GameDay data with the Lahman database
#' 
#' @details Cross-checks summary statistics with the Lahman database. 
#' 
#' @param data An MLBAM data set
#' 
#' @return The ratio of the Frobenius norm of the matrix of differences to the Frobenius norm of the matrix
#' defined by the Lahman database. 
#' 
#' @export
#' @examples
#' 
#' ds = getData()
#' crosscheck(ds)
#' 
crosscheck = function (data) UseMethod("crosscheck")

crosscheck.MLBAMData = function (data) {
  require(plyr)
  require(Lahman)
  data$bat_team = with(data, ifelse(half == "top", as.character(away_team), as.character(home_team)))
  data = transform(data, yearId = as.numeric(substr(gameId, start=5, stop=8)))
  teams = ddply(data, ~ yearId + bat_team, summarise, G = length(unique(gameId))
                , PA = sum(isPA), R = sum(runsOnPlay), H = sum(isHit), HR = sum(event == "Home Run")
                , K = sum(event %in% c("Strikeout", "Strikeout - DP"))
                , OBP = sum(isHit | event %in% c("Walk", "Intent Walk", "Hit By Pitch")) / sum(isPA & !event %in% c("Sac Bunt", "Sacrifice Bunt DP")))
  
  lteams = ddply(Batting, ~ yearID + teamID, summarise, PA = sum(AB + BB + HBP + SH + SF, na.rm=TRUE)
                 , R = sum(R, na.rm=TRUE), H = sum(H, na.rm=TRUE), HR = sum(HR, na.rm=TRUE)
                , K = sum(SO, na.rm=TRUE), OBP = sum(H + BB + HBP, na.rm=TRUE) / sum(AB + BB + HBP + SF, na.rm=TRUE))
  
  lteams = merge(x=lteams, y=Teams[,c("yearID", "teamID", "G")], by=c("yearID", "teamID"))
  lteams = transform(lteams, teamId = tolower(teamID))
  lteams$teamId = with(lteams, ifelse(teamId == "laa", "ana", as.character(teamId)))
  
  match = merge(x=teams, y=lteams, by.x=c("yearId", "bat_team"), by.y=c("yearID", "teamId"), all.x=TRUE)
  A = as.matrix(match[,c("G.x", "PA.x", "R.x", "H.x", "HR.x", "K.x")])
  B = as.matrix(match[,c("G.y", "PA.y", "R.y", "H.y", "HR.y", "K.y")])
  return(norm(A - B, "F") / norm(B, "F"))
}
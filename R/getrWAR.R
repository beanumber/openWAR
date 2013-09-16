#' @title getrWAR
#' 
#' @description Retrieves rWAR from Baseball-Reference.com
#' 
#' @details Retrieves daily rWAR figures from Baseball-Reference.com and stores it as a data.frame
#' 
#' @return a data.frame consisting of rWAR
#' 
#' @export
#' @examples
#' 
#' # Get data from yesterday
#' ds = getrWAR()
#' require(mosaic)
#' densityplot(~WAR, data=ds, plot.points=FALSE)
#' 
#' # Leaders since 1954
#' modern = subset(ds, yearId >= 1954)
#' head(modern[order(modern$WAR, decreasing=TRUE),], 20)
#' 
#' # Relationship between batting and fielding
#' xyplot(RAA_field ~ RAA_bat, data=modern, type=c("p", "r", "smooth"))

getrWAR <- function () {
  bat = "http://www.baseball-reference.com/data/war_daily_bat.txt"
  pitch = "http://www.baseball-reference.com/data/war_daily_pitch.txt"
  require(RCurl)
  rWAR.b = read.csv(bat, na.strings = c("NULL", "NA", ""))
  rWAR.p = read.csv(pitch, na.strings = c("NULL", "NA", ""))  
  
  # Grab the number of batters faced for pitchers
  bf.lkup = Lahman::Pitching[,c("playerID", "yearID", "teamID", "stint", "BFP")]
  rWAR.p = merge(x=rWAR.p, y=bf.lkup, by.x = c("player_ID", "year_ID", "stint_ID"), by.y = c("playerID", "yearID", "stint"), all.x=TRUE)
  
  bat.fields = c("player_ID", "year_ID", "stint_ID", "team_ID", "PA", "runs_position", "runs_above_rep", "runs_above_avg", "runs_above_avg_off", "runs_above_avg_def", "WAR")
  pitch.fields = c("player_ID", "year_ID", "stint_ID", "team_ID", "BFP", "runs_above_rep", "runs_above_avg", "WAR")
  rWAR = merge(x=rWAR.b[,bat.fields], y=rWAR.p[,pitch.fields], by=c("player_ID", "year_ID", "stint_ID"), all=TRUE)
  require(plyr)
  rWAR = rename(rWAR, replace=c("runs_above_avg_off" = "RAA_bat", "runs_above_avg_def" = "RAA_field"
                                , "runs_above_avg.y" = "RAA_pitch", "player_ID" = "playerId"
                                , "year_ID" = "yearId", "stint_ID" = "stintId"))
  rWAR$BF = with(rWAR, ifelse(is.na(PA), 0, PA) + ifelse(is.na(BFP), 0, BFP))
  rWAR$RAR = with(rWAR, ifelse(is.na(runs_above_rep.x), 0, runs_above_rep.x) + ifelse(is.na(runs_above_rep.y), 0, runs_above_rep.y))
  rWAR$RAA = with(rWAR, ifelse(is.na(runs_above_avg.x), 0, runs_above_avg.x) + ifelse(is.na(RAA_pitch), 0, RAA_pitch))  
  rWAR$repl = with(rWAR, RAA - RAR)
  rWAR$WAR = with(rWAR, ifelse(is.na(WAR.x), 0, WAR.x) + ifelse(is.na(WAR.y), 0, WAR.y))
  rWAR$teamId = with(rWAR, ifelse(is.na(team_ID.x), as.character(team_ID.y), as.character(team_ID.x)))
  out = rWAR[, setdiff(1:ncol(rWAR), grep("\\.(x|y)", names(rWAR)))]
  return(out)
}


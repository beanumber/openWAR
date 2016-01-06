
#' @title crosscheck
#' 
#' @description Cross-check the accuracy of the GameDay data with the Lahman database
#' 
#' @details Cross-checks summary statistics with the Lahman database. 
#' 
#' @param data \code{GameDayPlays} data frame
#' 
#' @return The ratio of the Frobenius norm of the matrix of differences to the Frobenius norm of the matrix
#' defined by the Lahman database. 
#' 
#' @export

crosscheck = function(data) UseMethod("crosscheck")

#' @rdname crosscheck
#' @method crosscheck GameDayPlays
#' @import dplyr
#' @export

crosscheck.GameDayPlays = function(data) {
  
  teams <- summary(data)
  
  lteams <- Lahman::Batting %>% 
    group_by_("yearID", "teamID") %>% 
    summarise_(PA = ~sum(AB + BB + HBP + SH + SF, na.rm = TRUE), 
               AB = ~sum(AB, na.rm = TRUE), 
               R = ~sum(R, na.rm = TRUE), H = ~sum(H, na.rm = TRUE), 
              HR = ~sum(HR, na.rm = TRUE), BB = ~sum(BB, na.rm = TRUE), 
              K = ~sum(SO, na.rm = TRUE), BA = ~sum(H, na.rm = TRUE)/sum(AB, na.rm = TRUE), 
              OBP = ~sum(H + BB + HBP, na.rm = TRUE)/sum(AB + BB + HBP + SF, na.rm = TRUE), 
              SLG = ~sum(H + X2B + X3B + HR, na.rm = TRUE)/sum(AB, na.rm = TRUE)
    )
  
  lteams <- merge(x = lteams, y = Lahman::Teams[, c("yearID", "teamID", "G")], by = c("yearID", "teamID"))
  lteams <- mutate_(lteams, teamId = ~tolower(teamID))
  lteams <- mutate_(lteams, teamId = ~ifelse(teamId == "laa", "ana", as.character(teamId)))
  
  match <- merge(x = teams, y = lteams, by.x = c("yearId", "bat_team"), by.y = c("yearID", "teamId"), all.x = TRUE)
  
  # move this out of here eventually 
  # require(xtable) x = xtable(match[,c('bat_team', 'G.x', 'PA.x', 'AB.x', 'R.x', 'H.x',
  # 'HR.x', 'BB.x', 'K.x', 'G.y', 'PA.y', 'AB.y', 'R.y', 'H.y', 'HR.y', 'BB.y', 'K.y')] , caption=c('Cross-check between MLBAM
  # data (left) and Lahman data (right), 2012'), label='tab:crosscheck' , align = rep('c', 18)) print(x,
  # include.rownames=FALSE)
  
  A <- as.matrix(match[, c("G.x", "PA.x", "AB.x", "R.x", "H.x", "HR.x", "BB.x", "K.x")])
  B <- as.matrix(match[, c("G.y", "PA.y", "AB.y", "R.y", "H.y", "HR.y", "BB.y", "K.y")])
  return(norm(A - B, "F")/norm(B, "F"))
}

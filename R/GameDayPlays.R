#' @title GameDayPlays
#' 
#' @description Contains the output from getData()
#' 
#' @exportClass GameDayPlays
#' @examples showClass("GameDayPlays")

setClass("GameDayPlays", contains = "data.frame")



#' @title panel.baseball
#' 
#' @description Visualize Balls in Play
#' 
#' @details A convenience function for drawing a generic baseball field using a Cartesian coordinate
#' system scaled in feet with home plate at the origin. 
#' 
#' 
#' @return nothing
#' 
#' @export
#' @examples
#' 
#' ds = getData()
#' plot(ds)

panel.baseball <- function () {
  bgcol = "darkgray"
  panel.segments(0, 0, -400, 400, col=bgcol)   # LF line
  panel.segments(0, 0, 400, 400, col=bgcol)     # RF line
  bw = 2
  # midpoint is at (0, 127.27)
  base2.y = sqrt(90^2 + 90^2)
  panel.polygon(c(-bw, 0, bw, 0), c(base2.y, base2.y - bw, base2.y, base2.y + bw), col=bgcol)
  # back corner is 90' away on the line
  base1.x = 90 * cos(pi/4)
  base1.y = 90 * sin(pi/4)
  panel.polygon(c(base1.x, base1.x - bw, base1.x - 2*bw, base1.x - bw), c(base1.y, base1.y - bw, base1.y, base1.y + bw), col=bgcol)
  # back corner is 90' away on the line
  base3.x = 90 * cos(3*pi/4)
  panel.polygon(c(base3.x, base3.x + bw, base3.x + 2*bw, base3.x + bw), c(base1.y, base1.y - bw, base1.y, base1.y + bw), col=bgcol)
  # infield cutout is 95' from the pitcher's mound
  panel.curve(60.5 + sqrt(95^2 - x^2), from=base3.x - 26, to=base1.x + 26, col=bgcol)
  # pitching rubber
  panel.rect(-bw, 60.5 - bw/2, bw, 60.5 + bw/2, col=bgcol)
  # home plate
  panel.polygon(c(0, -8.5/12, -8.5/12, 8.5/12, 8.5/12), c(0, 8.5/12, 17/12, 17/12, 8.5/12), col=bgcol)
  # distance curves
  distances = seq(from=200, to=500, by = 100)
  for (i in 1:length(distances)) {
    d = distances[i]
  panel.curve(sqrt(d^2 - x^2), from= d * cos(3*pi/4), to=d * cos(pi/4), col=bgcol)
  }
}



#' @title plot.GameDayPlays
#' 
#' @description Visualize Balls in Play
#' 
#' @details Plots the balls in play from GameDay data. This function will plot (x,y)-coordinates
#' with a generic baseball field plotted in the background. Other lattice options can be passed
#' to xyplot().
#' 
#' @param data A GameDayPlays set with fields "our.x" and "our.y"
#' @param batterName A character string containing the last name of a batter
#' @param pitcherName A character string containing the last name of a pitcher 
#' @param pch A numeric 
#' 
#' @return an xyplot() 
#' 
#' @export
#' @examples
#' 
#' ds = getData()
#' plot(ds)

plot.GameDayPlays = function (data, batterName=NULL,pitcherName=NULL,event=NULL,pch=1) {
  require(mosaic)
  xy.fields = c("our.x", "our.y")
  if (!length(intersect(xy.fields, names(data))) == length(xy.fields)) {
    stop("(x,y) coordinate locations not found.")
  }
  #Code for filtering base on batter, pitcher and/or event type.  
  if (!is.null(batterName)) { 
    data = data[data$batterName==batterName,]
  }
  if (!is.null(pitcherName)) {
    data = data[data$pitcherName==pitcherName,]
  }
  if (!is.null(event)) {
    data = data[data$event %in% event,]
  }
  ds <- filter(data, !is.na(our.y) & !is.na(our.x))
  ds$event <- factor(ds$event)
  plot = xyplot(our.y ~ our.x, groups=event, data=ds,pch=pch
         , panel = function(x,y, ...) {
           panel.baseball()
           panel.xyplot(x,y, alpha = 0.3, ...)
         }
       , auto.key=list(columns=4)
       , xlim = c(-350, 350), ylim = c(-20, 525)
       , xlab = "Horizontal Distance from Home Plate (ft.)"
       , ylab = "Vertical Distance from Home Plate (ft.)"
  )
  return(plot)
}

#' @title summary.GameDayPlays
#' 
#' @description Summarize MLBAM data
#' 
#' @details Prints information about the contents of an GameDayPlays data set.
#' 
#' @param data A GameDayPlays data set
#' 
#' @return nothing
#' 
#' @export
#' @examples
#' 
#' ds = getData()
#' summary(ds)

summary.GameDayPlays = function (data) {
  gIds = sort(unique(data$gameId))
  message(paste("...Contains data from", length(gIds), "games"))
  message(paste("...from", gIds[1], "to", gIds[length(gIds)]))
  summary.data.frame(data)
}

#' @title tabulate.GameDayPlays
#' 
#' @description Summarize MLBAM data
#' 
#' @details Tabulates Lahman-style statistics by team for the contents of a GameDayPlays data set.
#' 
#' @param data A GameDayPlays set
#' 
#' @return A data.frame of seasonal totals for each team
#' 
#' @export tabulate.GameDayPlays
#' @examples
#' 
#' ds = getData()
#' tabulate(ds)

tabulate = function (data) UseMethod("tabulate")

tabulate.GameDayPlays = function (data) {
#  data$bat_team = with(data, ifelse(half == "top", as.character(away_team), as.character(home_team)))
  
#  data <- mutate(data, yearId = as.numeric(substr(gameId, start=5, stop=8)))
#   teams = plyr::ddply(data, ~ yearId + bat_team, summarise, G = length(unique(gameId))
#                 , PA = sum(isPA), AB = sum(isAB), R = sum(runsOnPlay), H = sum(isHit)
#                 , HR = sum(event == "Home Run")
#                 , BB = sum(event %in% c("Walk", "Intent Walk"))
#                 , K = sum(event %in% c("Strikeout", "Strikeout - DP"))
#                 , BA = sum(isHit) / sum(isAB)
#                 , OBP = sum(isHit | event %in% c("Walk", "Intent Walk", "Hit By Pitch")) / sum(isPA & !event %in% c("Sac Bunt", "Sacrifice Bunt DP"))
#                 , SLG = (sum(event == "Single") + 2*sum(event == "Double") + 3*sum(event == "Triple") + 4*sum(event == "Home Run") ) / sum(isAB)
#   )
  
  data %>%
    mutate(bat_team = ifelse(half == "top", as.character(away_team), as.character(home_team))) %>%
    mutate(yearId = as.numeric(substr(gameId, start=5, stop=8))) %>%
    group_by(yearId, bat_team) %>%
    summarise(G = length(unique(gameId))                
              , PA = sum(isPA)
              , AB = sum(isAB)
              , R = sum(runsOnPlay)
              , H = sum(isHit)
              , HR = sum(event == "Home Run")
              , BB = sum(event %in% c("Walk", "Intent Walk"))
              , K = sum(event %in% c("Strikeout", "Strikeout - DP"))
              , BA = sum(isHit) / sum(isAB)
              , OBP = sum(isHit | event %in% c("Walk", "Intent Walk", "Hit By Pitch")) / sum(isPA & !event %in% c("Sac Bunt", "Sacrifice Bunt DP"))
              , SLG = (sum(event == "Single") + 2*sum(event == "Double") + 3*sum(event == "Triple") + 4*sum(event == "Home Run") ) / sum(isAB)
    )
}


#' @title crosscheck.GameDayPlays
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
#' @export crosscheck.GameDayPlays
#' @examples
#' 
#' ds = getData()
#' crosscheck(ds)
#' 
#' 
crosscheck = function (data) UseMethod("crosscheck")

crosscheck.GameDayPlays = function (data) {
  require(Lahman)
  
  teams = tabulate(data)
  
  lteams <- Batting %>%
    group_by(yearID, teamID) %>%
    summarise(PA = sum(AB + BB + HBP + SH + SF, na.rm=TRUE)
              , AB = sum(AB, na.rm=TRUE)
              , R = sum(R, na.rm=TRUE)
              , H = sum(H, na.rm=TRUE)
              , HR = sum(HR, na.rm=TRUE)
              , BB = sum(BB, na.rm=TRUE)
              , K = sum(SO, na.rm=TRUE)
              , BA = sum(H, na.rm=TRUE) / sum(AB, na.rm=TRUE)
              , OBP = sum(H + BB + HBP, na.rm=TRUE) / sum(AB + BB + HBP + SF, na.rm=TRUE)
              , SLG = sum(H + X2B + X3B + HR, na.rm=TRUE) / sum(AB, na.rm=TRUE))
  
  lteams = merge(x=lteams, y=Teams[,c("yearID", "teamID", "G")], by=c("yearID", "teamID"))
  lteams <- mutate(lteams, teamId = tolower(teamID))
  lteams <- mutate(lteams, teamId = ifelse(teamId == "laa", "ana", as.character(teamId)))
  
  match = merge(x=teams, y=lteams, by.x=c("yearId", "bat_team"), by.y=c("yearID", "teamId"), all.x=TRUE)
  
  # move this out of here eventually
  #  require(xtable)
  #  x = xtable(match[,c("bat_team", "G.x", "PA.x", "AB.x", "R.x", "H.x", "HR.x", "BB.x", "K.x", "G.y", "PA.y", "AB.y", "R.y", "H.y", "HR.y", "BB.y", "K.y")]
  #             , caption=c("Cross-check between MLBAM data (left) and Lahman data (right), 2012"), label="tab:crosscheck"
  #             , align = rep("c", 18))
  #  print(x, include.rownames=FALSE)
  
  A = as.matrix(match[,c("G.x", "PA.x", "AB.x", "R.x", "H.x", "HR.x", "BB.x", "K.x")])
  B = as.matrix(match[,c("G.y", "PA.y", "AB.y", "R.y", "H.y", "HR.y", "BB.y", "K.y")])
  return(norm(A - B, "F") / norm(B, "F"))
}






#' @title shakeWAR
#' 
#' @description resample a data.frame to obtain variance estimate for WAR
#' 
#' @details Resamples the rows of an MLBAM data set
#' 
#' @param data An MLBAM data.frame 
#' @param resample An element of \code{c("plays", "models", "both")}
#' @param N the number of resamples (default 5000)
#' 
#' @return a data.frame with RAA values 
#' 
#' @export shakeWAR
#' @export shakeWAR.GameDayPlays
#' @examples
#' 
#' ds = getData()
#' res = shakeWAR(ds, resample="plays", N=10)
#' summary(res)
#' 

shakeWAR = function (data, resample = "plays", N = 10, ...) UseMethod("shakeWAR")

shakeWAR.GameDayPlays = function (data, resample = "plays", N = 10, ...) {
  require(mosaic)
  
  if (resample == "both") {
    # resample the actual plays AND rebuild the models each time
    # this captures both measurement error and sampling error
    bstrap = do(N) * getWAR(makeWAR(resample(data), low.memory=TRUE)$openWAR)
  } else {
    
    ext = makeWAR(data, verbose=FALSE, low.memory=TRUE)
    # Keep track of the original data
    reality = data
    # Keep track of the models built on the original data
    reality.models = ext$models
    # Keep track of the original RAA values
    reality.raa = ext$openWAR
    
    if (resample == "plays") {
      # assume the models are fixed, and resample the RAA values
      # this captures the sampling error
      
      # supposedly the performance of do() is really bad
      bstrap = do(N) * getWAR(resample(reality.raa), verbose=FALSE)
      # use replicate() instead
      #    bstrap = rdply(N, getWAR(resample(reality.raa), verbose=FALSE))
      #    class(bstrap) = c("do.openWARPlayers", class(bstrap))
#     } else {
#         # to isolate the measurement error, use the models we built on the resampled rows
#         # but apply them exclusively to the real data
#         ext.list = lapply(sims$models.used, makeWAR, data = reality, verbose=FALSE)
#         raa.list = lapply(ext.list, "[[", "openWAR")
#         war.list = t(lapply(raa.list, getWAR))
#         bstrap = do.call("rbind", war.list)
#         class(bstrap) = c("do.openWARPlayers", class(bstrap))
    }
  }
  
  # bstrap should be a data.frame of class "do.openWARPlayers"
  class(bstrap) <- c("do.openWARPlayers", "data.frame")
  # with roughly N * M rows, where M is the numbers of players
  return(bstrap)
}


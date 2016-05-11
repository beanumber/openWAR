utils::globalVariables("event")

#' @title An object containing play-by-play data
#' 
#' @description A data set that contains play-by-play information returned by \code{\link{getData}}
#' 
#' @format A \code{\link{GameDayPlays}} object. This is a \code{\link{tbl_df}} and 
#' \code{\link{data.frame}} with 62 columns. The columns are as follows:
#'      \describe{
#'        \item{pitcherId}{The MLBAM id of the pitcher}
#'        \item{batterId}{The MLBAM id of the batter}
#'         \item{field_teamId}{The MLBAM id of the fielding team}
#'         \item{ab_num}{The chronological number of the plate appearance}
#'         \item{inning}{The inning number}
#'         \item{half}{Indicates which half of the inning (i.e. top or bottom)}
#'         \item{balls}{The number of balls at the end of the plate appearance}
#'         \item{strikes}{The number of strikes at the end of the plate appearance}
#'         \item{endOuts}{The number of outs at the end of the plate appearance}
#'         \item{event}{The result of the plate appearance (e.g. Single, Walk, Grounded Into DP)}
#'         \item{actionID}{NA}
#'         \item{description}{A description of the result of the plate appearance}
#'         \item{stand}{The handedness of the batter during the plate appearance}
#'         \item{throws}{The handedness of the pitcher}
#'         \item{runnerMovement}{A coded description of movement of all base runners during the plate appearance}
#'         \item{x}{For balls in play, the MLBAM x-coordinate of where the ball 
#'         is first touched. Note that 0 is the far left hand edge of the 
#'         stadium image file. (NA for balls not in play)}
#'         \item{y}{For balls in play, the MLBAM y-coordinate of where the ball 
#'         is first touched. Note that 0 is the top of the stadium image file. 
#'         (NA for balls not in play)}
#'         \item{game_type}{Game type}
#'         \item{home_team}{The MLBAM home team abbreviation}
#'         \item{home_teamId}{The MLBAM id of the home team}
#'         \item{home_lg}{The league of the home team (i.e. AL or NL)}
#'         \item{away_team}{The MLBAM away team abbreviation}
#'         \item{away_teamId}{The MLBAM id of the away team}
#'         \item{away_lg}{The league of the away team (i.e. AL or NL)}
#'         \item{venueId}{The MLBAM park id}
#'         \item{stadium}{The name of the stadium where the game is being played}
#'         \item{timestamp}{The timestamp associated withe the plate appearance}
#'         \item{playerId.C}{The MLBAM of the catcher}
#'         \item{playerId.1B}{The MLBAM of the first baseman}
#'         \item{playerId.2B}{The MLBAM of the second baseman}
#'         \item{playerId.3B}{The MLBAM of the third baseman}
#'         \item{playerId.SS}{The MLBAM of the shortstop}
#'         \item{playerId.LF}{The MLBAM of the left fielder}
#'         \item{playerId.CF}{The MLBAM of the center fielder}
#'         \item{playerId.RF}{The MLBAM of the right fielder}
#'         \item{batterPos}{The defensive postion of the current batter}
#'         \item{batterName}{The name of the current batter}
#'         \item{pitcherName}{The name of the current pitcher}
#'         \item{runsOnPlay}{The number of runs that were scored as a result of the plate appearance}
#'         \item{startOuts}{The number of outs at the beginning of the plate appearnace}
#'         \item{runsInInning}{The total number of runs scored in the half of an inning}
#'         \item{runsITD}{The total number of runs that have been scored in the half of an inning prior to the current plate appearance}
#'         \item{runsFuture}{The total number of runs that are scored in the half of an inning that were scored after the current plate appearance}
#'         \item{startCode}{Binary representation of the base runner configuration at the start of the plate appearance}
#'         \item{endCode}{Binary representation of the base runner configuration at the end of the plate appearance}
#'         \item{fielderId}{For a ball in play, the MLBAM id of the fielder who first touches the baseball. (NA for balls not in play) }
#'         \item{endCode}{Binary representation of the base runner configuration at the end of the plate appearance}
#'         \item{gameId}{The MLBAM id for this game}
#'         \item{isPA}{Boolean indicating if the plate appearance was a plate appearance}
#'         \item{isAB}{Boolean indicating if the plate appearance was an at bat}
#'         \item{isHit}{Boolean indicating if the plate appearance resulted in a hit}
#'         \item{isBIP}{Boolean indicating if the plate appearance resulted in a ball in play}
#'         \item{our.x}{Horizontal coordinate, in feet, of where the ball landed.
#'         Home plate has \code{our.x} of 0.}
#'         \item{out.y}{Vertical coordinate, in feet, of where the ball landed. 
#'         Home plate has \code{our.y} of 0.}
#'         \item{r}{radial distance, in feet, of the location of the batted ball. 
#'         Home plate has \code{r} of 0.}
#'         \item{theta}{radial angle, in radians, of the location of the batted ball.
#'         Home plate has \code{theta} of 0.}
#'      }
#' @param x an object
#' @import methods
#' @examples 
#' class(May)
#' 

GameDayPlays <- function(x) { 
  class(x) <- c("GameDayPlays", class(x))
}



#' @title panel.baseball
#' 
#' @description Visualize balls in play
#' 
#' @details A convenience function for drawing a generic baseball field using a Cartesian coordinate
#' system scaled in feet with home plate at the origin. 
#' 
#' 
#' @return NULL
#' 
#' @import lattice
#' 
#' @export
#' @rdname plot.GameDayPlays

panel.baseball <- function() {
    bgcol = "darkgray"
    panel.segments(0, 0, -400, 400, col = bgcol)  # LF line
    panel.segments(0, 0, 400, 400, col = bgcol)  # RF line
    bw = 2
    # midpoint is at (0, 127.27)
    base2.y = sqrt(90^2 + 90^2)
    panel.polygon(c(-bw, 0, bw, 0), c(base2.y, base2.y - bw, base2.y, base2.y + bw), col = bgcol)
    # back corner is 90' away on the line
    base1.x = 90 * cos(pi/4)
    base1.y = 90 * sin(pi/4)
    panel.polygon(c(base1.x, base1.x - bw, base1.x - 2 * bw, base1.x - bw), c(base1.y, base1.y - bw, base1.y, base1.y + bw), 
        col = bgcol)
    # back corner is 90' away on the line
    base3.x = 90 * cos(3 * pi/4)
    panel.polygon(c(base3.x, base3.x + bw, base3.x + 2 * bw, base3.x + bw), c(base1.y, base1.y - bw, base1.y, base1.y + bw), 
        col = bgcol)
    # infield cutout is 95' from the pitcher's mound
    x <- NULL; rm(x); # Dummy to trick R CMD check 
    # http://r.789695.n4.nabble.com/R-CMD-check-tells-me-no-visible-binding-for-global-variable-what-does-it-mean-td1837236.html
    panel.curve(60.5 + sqrt(95^2 - x^2), from = base3.x - 26, to = base1.x + 26, col = bgcol)
    # pitching rubber
    panel.rect(-bw, 60.5 - bw/2, bw, 60.5 + bw/2, col = bgcol)
    # home plate
    panel.polygon(c(0, -8.5/12, -8.5/12, 8.5/12, 8.5/12), c(0, 8.5/12, 17/12, 17/12, 8.5/12), col = bgcol)
    # distance curves
    distances = seq(from = 200, to = 500, by = 100)
    for (i in 1:length(distances)) {
        d = distances[i]
        panel.curve(sqrt(d^2 - x^2), from = d * cos(3 * pi/4), to = d * cos(pi/4), col = bgcol)
    }
}



#' @title plot.GameDayPlays
#' 
#' @description Visualize Balls in Play
#' 
#' @details Plots the balls in play from an object of class \code{\link{GameDayPlays}}. This function will plot (x,y)-coordinates
#' with a generic baseball field plotted in the background. Other lattice options can be passed
#' to \code{\link{xyplot}}.
#' 
#' @param x An object of class \code{\link{GameDayPlays}}
#' @param batterName A character string containing the last name of a batter
#' @param pitcherName A character string containing the last name of a pitcher 
#' @param events A vector of MLBAM event types for which to filter. (e.g. 'Home Run')
#' @param ... arguments passed to \code{\link{panel.xyplot}}
#' 
#' @return an xyplot() 
#' 
#' @import lattice
#' 
#' @export
#' @examples
#' 
#' plot(May)
#' plot(May, events = c("Single","Double","Triple","Home Run"), pch = 16)
#' plot(May, batterName = "Trout", main = "Mike Trout's May 2013", pch = 16)
#' plot(May, pitcherName = "Kershaw", main = "Clayton Kershaw's May 2013", pch = 16)
#' plot(May, batterName = "Tulowitzki", pitcherName = "Kershaw", 
#'      main = "Clayton Kershaw versus Troy Tulowitzki: May 2013", pch = 16, cex = 3)



plot.GameDayPlays = function(x, batterName = NULL, pitcherName = NULL, events = NULL, ...) {
    xy.fields <- c("our.x", "our.y")
    if (!length(intersect(xy.fields, names(x))) == length(xy.fields)) {
        stop("(x,y) coordinate locations not found.")
    }
    # Code for filtering base on batter, pitcher and/or event type.
    if (!is.null(batterName)) {
        x = x[x$batterName == batterName, ]
    }
    if (!is.null(pitcherName)) {
        x = x[x$pitcherName == pitcherName, ]
    }
    if (!is.null(events)) {
        x = x[x$event %in% events, ]
    }
    ds <- filter_(x, ~!is.na(our.y) & !is.na(our.x))
    ds <- droplevels(ds)
    nkeycols <- min(5, nlevels(factor(ds$event)))
    plot = lattice::xyplot(our.y ~ our.x
                  , groups = event
                  , data = ds
                  , ...
                  , panel = function(x, y, ...) {
                    panel.baseball()
                    panel.xyplot(x, y, alpha = 0.3, ...)
                    }
                  , auto.key = list(columns = nkeycols)
                  , xlim = c(-350, 350), ylim = c(-20, 525)
                  , xlab = "Horizontal Distance from Home Plate (ft.)"
                  , ylab = "Vertical Distance from Home Plate (ft.)"
                  )
    return(plot)
}

#' @title Summarize a GameDayPlays data set
#' 
#' @description Summarize MLBAM data
#' 
#' @details Tabulates cumulative statistics by team for the contents of a \code{\link{GameDayPlays}} data set.
#' 
#' @param object An object of class \code{\link{GameDayPlays}}
#' @param ... currently ignored
#' 
#' @return A \code{\link{tbl_df}} of totals for each team
#' 
#' @export 
#' @examples
#' 
#' summary(May)
#' 

summary.GameDayPlays = function(object, ...) {
  
  gIds = sort(unique(object$gameId))
  message(paste("...Contains data from", length(gIds), "unique games"))
  message(paste("...from", first(gIds), "to", last(gIds)))
  
  object %>% 
    mutate_(bat_team = ~factor(ifelse(half == "top", as.character(away_team), as.character(home_team)))) %>% 
    mutate_(yearId = ~as.numeric(substr(gameId, start = 5, stop = 8))) %>% 
    group_by_(~yearId, ~bat_team) %>% 
    summarise_(G = ~length(unique(gameId)), PA = ~sum(isPA), AB = ~sum(isAB), 
               R = ~sum(runsOnPlay), 
               H = ~sum(isHit), 
               X2B = ~sum(event == "Double"), 
               X3B = ~sum(event == "Triple"), 
               HR = ~sum(event == "Home Run"), 
               BB = ~sum(event %in% c("Walk", "Intent Walk")), 
               K = ~sum(event %in% c("Strikeout", "Strikeout - DP")), 
               BAvg = ~sum(isHit)/sum(isAB), 
               OBP = ~sum(isHit | event %in% c("Walk", "Intent Walk", "Hit By Pitch"))/sum(isPA & !event %in% c("Sac Bunt", "Sacrifice Bunt DP")), 
               SLG = ~(sum(event == "Single") + 2 * sum(event == "Double") + 3 * sum(event == "Triple") + 4 * sum(event == "Home Run"))/sum(isAB)
    ) %>%
    mutate_(OPS = ~OBP + SLG)
}

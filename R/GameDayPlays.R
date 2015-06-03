#' @title GameDayPlays
#' 
#' @description Contains the output from getData()
#' 
#' @exportClass GameDayPlays
#' @examples showClass('GameDayPlays')

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
#' @importFrom lattice panel.segments
#' @importFrom lattice panel.polygon
#' @importFrom lattice panel.curve
#' @importFrom lattice panel.rect
#' 
#' @export
#' @examples
#' 
#' ds = getData()
#' plot(ds)

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
#' @details Plots the balls in play from GameDay data. This function will plot (x,y)-coordinates
#' with a generic baseball field plotted in the background. Other lattice options can be passed
#' to xyplot().
#' 
#' @param x A GameDayPlays data set with fields 'our.x' and 'our.y'
#' @param batterName A character string containing the last name of a batter
#' @param pitcherName A character string containing the last name of a pitcher 
#' @param pch A numeric 
#' 
#' @return an xyplot() 
#' 
#' @importFrom lattice xyplot
#' @importFrom lattice panel.xyplot
#' 
#' @export
#' @examples
#' 
#' ds = getData()
#' plot(ds)

plot.GameDayPlays = function(x, batterName = NULL, pitcherName = NULL, event = NULL, pch = 1, ...) {
    data = x
    xy.fields = c("our.x", "our.y")
    if (!length(intersect(xy.fields, names(data))) == length(xy.fields)) {
        stop("(x,y) coordinate locations not found.")
    }
    # Code for filtering base on batter, pitcher and/or event type.
    if (!is.null(batterName)) {
        data = data[data$batterName == batterName, ]
    }
    if (!is.null(pitcherName)) {
        data = data[data$pitcherName == pitcherName, ]
    }
    if (!is.null(event)) {
        data = data[data$event %in% event, ]
    }
    ds <- filter(data, !is.na(our.y) & !is.na(our.x))
    ds$event <- factor(ds$event)
    plot = xyplot(our.y ~ our.x, groups = event, data = ds, pch = pch
                  , panel = function(x, y, ...) {
                    panel.baseball()
                    panel.xyplot(x, y, alpha = 0.3, ...)
                    }
                  , auto.key = list(columns = 4)
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
#' @param object A GameDayPlays data set
#' 
#' @return nothing
#' 
#' @export
#' @examples
#' 
#' ds = getData()
#' summary(ds)

summary.GameDayPlays = function(object, ...) {
    gIds = sort(unique(object$gameId))
    message(paste("...Contains data from", length(gIds), "games"))
    message(paste("...from", gIds[1], "to", gIds[length(gIds)]))
    summary.data.frame(object)
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
#' @export tabulate
#' @examples
#' 
#' ds = getData()
#' tabulate(ds)

tabulate = function(data) UseMethod("tabulate")

tabulate.GameDayPlays = function(data) {
    # data$bat_team = with(data, ifelse(half == 'top', as.character(away_team), as.character(home_team)))
    
    # data <- mutate(data, yearId = as.numeric(substr(gameId, start=5, stop=8))) teams = plyr::ddply(data, ~ yearId + bat_team,
    # summarise, G = length(unique(gameId)) , PA = sum(isPA), AB = sum(isAB), R = sum(runsOnPlay), H = sum(isHit) , HR =
    # sum(event == 'Home Run') , BB = sum(event %in% c('Walk', 'Intent Walk')) , K = sum(event %in% c('Strikeout', 'Strikeout -
    # DP')) , BA = sum(isHit) / sum(isAB) , OBP = sum(isHit | event %in% c('Walk', 'Intent Walk', 'Hit By Pitch')) / sum(isPA &
    # !event %in% c('Sac Bunt', 'Sacrifice Bunt DP')) , SLG = (sum(event == 'Single') + 2*sum(event == 'Double') + 3*sum(event
    # == 'Triple') + 4*sum(event == 'Home Run') ) / sum(isAB) )
    
    data %>% 
      mutate(bat_team = factor(ifelse(half == "top", as.character(away_team), as.character(home_team)))) %>% 
      mutate(yearId = as.numeric(substr(gameId, start = 5, stop = 8))) %>% 
      group_by(yearId, bat_team) %>% 
      summarise(G = length(unique(gameId)), PA = sum(isPA), AB = sum(isAB), 
        R = sum(runsOnPlay), H = sum(isHit), HR = sum(event == "Home Run"), 
        BB = sum(event %in% c("Walk", "Intent Walk")), K = sum(event %in% c("Strikeout", "Strikeout - DP")), 
        BA = sum(isHit)/sum(isAB), 
        OBP = sum(isHit | event %in% c("Walk", "Intent Walk", "Hit By Pitch"))/sum(isPA & !event %in% c("Sac Bunt", "Sacrifice Bunt DP")), 
        SLG = (sum(event == "Single") + 2 * sum(event == "Double") + 3 * sum(event == "Triple") + 4 * sum(event == "Home Run"))/sum(isAB)
        )
}

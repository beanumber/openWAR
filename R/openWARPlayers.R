#' @title Players' openWAR values
#' 
#' @description A \code{\link{data.frame}} of players and their tabulated openWAR values.  The
#'  function \code{\link{getWAR}} returns an object of class \code{\link{openWARPlayers}}.
#' 
#' @param x an object
#' @examples 
#' class(openWAR2012)
#' 
#' @format An \code{\link{openWARPlayers}} object.
#' \describe{
#'    \item{playerId}{The MLBAM id for the player}
#'    \item{PA.bat}{The number of plate appearances as a batter for that player}
#'    \item{G}{The number of games the player appeared in}
#'    \item{HR}{The number of home runs hit by that player}
#'    \item{RAA.bat}{Runs aboved average produced by this player from batting}
#'    \item{PA.br1}{The number of plate appearances that the player was a base runner on first base}
#'    \item{RAA.br1}{Runs above average produced by this player when they were a base runner on first base}
#'    \item{PA.br2}{The number of plate appearances that the player was a base runner on second base}
#'    \item{RAA.br2}{Runs above average produced by this player when they were a base runner on second base}
#'    \item{PA.br3}{The number of plate appearances that the player was a base runner on third base}
#'    \item{RAA.br3}{Runs above average produced by this player when they were a base runner on third base}
#'    \item{BF}{Number of batters faced by the player as a pitcher}
#'    \item{RAA.pitch}{Runs above average produced by this player from pitching}
#'    \item{Name}{Name of the player}
#'    \item{PA.P}{The number of plate appearances that the player's defensive position was pitcher}
#'    \item{RAA.P}{Runs above average produced by this player from fielding as a pitcher}
#'    \item{PA.C}{The number of plate appearances that the player's defensive position was catcher}
#'    \item{RAA.C}{Runs above average produced by this player from fielding as a catcher}
#'    \item{PA.1B}{The number of plate appearances that the player's defensive position was first base}
#'    \item{RAA.1B}{Runs above average produced by this player from fielding as a first base}
#'    \item{PA.2B}{The number of plate appearances that the player's defensive position was second base}
#'    \item{RAA.2B}{Runs above average produced by this player from fielding as a second base}
#'    \item{PA.3B}{The number of plate appearances that the player's defensive position was third base}
#'    \item{RAA.3B}{Runs above average produced by this player from fielding as a third base}
#'    \item{PA.SS}{The number of plate appearances that the player's defensive position was shortstop}
#'    \item{RAA.SS}{Runs above average produced by this player from fielding as a shortstop}
#'    \item{PA.LF}{The number of plate appearances that the player's defensive position was left field}
#'    \item{RAA.LF}{Runs above average produced by this player from fielding as a left field}
#'    \item{PA.CF}{The number of plate appearances that the player's defensive position was center field}
#'    \item{RAA.CF}{Runs above average produced by this player from fielding as a center field}
#'    \item{PA.RF}{The number of plate appearances that the player's defensive position was right field}
#'    \item{RAA.RF}{Runs above average produced by this player from fielding as a right field}
#'    \item{RAA.br}{Runs above average produced by this player as a base runner. Sum of RAA.br1, RAA.br2, and RAA.br3}
#'    \item{RAA.off}{Runs above average produced by this player as an offensive player. Sum of RAA.br and RAA.off}
#'    \item{RAA.field}{Runs above average produced by this player as a fielder. Sum of RAA.P, RAA.C, RAA.1B, RAA.2B, RAA.3B, RAA.SS, RAA.LF, RAA.CF, and RAA.RF}
#'    \item{RAA}{Total runs above average produced by this player.  Sum of RAA.bat, RAA.br, RAA.field, and RAA.pitch}
#'    \item{TPA}{Total plate appearances of the player as a batter or pitcher.  Sum of PA.bat and BF}
#'    \item{repl}{Runs above average produced by a shadow replacement-level player with the same profile (i.e. same number of batting, pitching, base running, and fielding opportunities) as the actual player}
#'    \item{WAR}{Wins above replacement for the player.  Calculated as the difference between RAA and repl divided by 10}
#'    \item{isReplacement}{A boolean indicating if the player was part of the replacement pool for calculating replacement level}
#'    }

openWARPlayers <- function(x) { 
  class(x) <- c("openWARPlayers", class(x))
}

#' @title Summarize openWAR across players
#' 
#' @description Summarize openWAR across players
#' 
#' @details A summary of players' WAR
#' 
#' @param object An object of class \code{\link{openWARPlayers}}
#' @param ... currently ignored
#' 
#' @import dplyr
#' @export
#' @examples
#' 
#' \dontrun{
#' MayProcessed = makeWAR(May)
#' }
#' war <- getWAR(MayProcessed$openWAR)
#' summary(war)
#' summary(openWAR2012)
#' summary(openWAR2013)

summary.openWARPlayers = function(object, ...) {
  message(paste("Displaying information for", nrow(object), "players, of whom", 
            nrow(dplyr::filter_(object, ~RAA.pitch != 0)), "have pitched"))
  
  object %>% 
    select_(~playerId, ~Name, ~TPA, ~WAR, ~RAA, ~repl, ~RAA.bat, ~RAA.br, ~RAA.field, ~RAA.pitch) %>%
    arrange_(~desc(WAR)) %>%
    tbl_df()
}




#' @title Display a season's worth of openWAR results
#' 
#' @description Display a season's worth of openWAR results
#' 
#' @details Given an \code{\link{openWARPlayers}} object, draw a plot displaying each player's RAA, WAR, and replacement
#' level shadow. 
#' 
#' @param x A data.frame object of class \code{\link{openWARPlayers}}
#' @param ... arguments passed to \code{\link{xyplot}}.
#' 
#' @export
#' 
#' @examples
#' 
#' \dontrun{
#' MayProcessed = makeWAR(May)
#' }
#' war = getWAR(MayProcessed$openWAR)
#' summary(war)
#' plot(war)

plot.openWARPlayers = function(x, ...) {
    data = x
    # Add the combined playing time
    data = dplyr::mutate_(data, TPA = ~PA.bat + BF)
    
    supp = data[, c("playerId", "Name", "WAR", "TPA", "repl", "RAA", "RAA.pitch")]
    names(supp) = c("playerId", "Name", "WAR", "TPA", "repl", "RAA", "RAA_pitch")
    
    p = xyplot(RAA ~ TPA, groups = ~isReplacement, data = data, panel = panel.war
               , data2 = supp, alpha = 0.3, pch = 19, type = c("p", "r")
               , par.settings = list(superpose.symbol = list(pch = 19))
               , ylab = "openWAR Runs Above Average"
               , xlab = "Playing Time (plate appearances plus batters faced)"
               , auto.key = list(columns = 2, corner = c(0.05, 0.95)
                                 , text = c("MLB Player", "Replacement Player"))
                                 , sub = paste("Number of Players =", nrow(data)
                                               , ", Number of Replacement-Level Players =", sum(data$isReplacement))
               , ...)
    print(p)
}

#' @title panel function for displaying openWAR
#' 
#' @description panel function for displaying openWAR
#' 
#' @details Given an \code{\link{openWARPlayers}} object, draw a plot displaying each player's RAA, WAR, and replacement
#' level shadow. 
#' 
#' @param x the x-coordinate
#' @param y the y-coordinate
#' @param ... arguments passed from \code{\link{plot.openWARPlayers}}
#' 
#' @export panel.war
#' @import dplyr
#' @import lattice
#' 
#' @examples
#' 
#' \dontrun{
#' MayProcessed = makeWAR(May)
#' }
#' war = getWAR(MayProcessed$openWAR)
#' summary(war)
#' plot(war)

panel.war = function(x, y, ...) {
    panel.abline(h = 0, col = "black")
    panel.xyplot(x, y, ...)
    # data2 is passed to the panel function via the ellipses, so extract those arguments vial match.call
    args <- match.call(expand.dots = FALSE)$...
    ds = args$data2
    panel.xyplot(ds$TPA, ds$repl, col = "darkgray", ...)
    # annotate the best player
    best.idx = which.max(ds$WAR)
    with(ds[best.idx, ], panel.arrows(TPA, repl, TPA, RAA, code = 3, lwd = 2, col = "darkgray", length = 0.1))
    with(ds[best.idx, ], panel.text(TPA, RAA, Name, pos = 4))
    # annotate the best pitcher
    pitchers = filter_(ds, ~RAA_pitch > 0)
    pitcher.idx = which.max(pitchers$WAR)
    with(pitchers[pitcher.idx, ], panel.arrows(TPA, repl, TPA, RAA, code = 3, lwd = 2, col = "darkgray", length = 0.1))
    with(pitchers[pitcher.idx, ], panel.text(TPA, RAA, Name, pos = 3))
    # annotate the worst player
    worst.idx = which.min(ds$WAR)
    with(ds[worst.idx, ], panel.arrows(TPA, repl, TPA, RAA, code = 3, lwd = 2, col = "darkgray", length = 0.1))
    with(ds[worst.idx, ], panel.text(TPA, RAA, Name, pos = 2))
    # annotate the total WAR in the system
    panel.text(0, ds[best.idx, "RAA"] * 0.6, paste("Total RAA =", round(sum(y), 1)), adj = 0)
    panel.text(0, ds[best.idx, "RAA"] * 0.6 - 3, paste("Total WAR =", round(sum(ds$WAR), 1)), adj = 0)
}


############################################################## Generic functions for bootstrapped results


#' @title Summarize resampled openWAR
#' 
#' @description Summarize resampled openWAR
#' 
#' @details Summary of players' WAR
#' 
#' @param object An object of class \code{\link{openWARPlayers}}
#' @param ... currently ignored
#' 
#' @import dplyr
#'
#' @export
#' 
#' @examples
#' 
#' \dontrun{
#' sim = shakeWAR(May)
#' summary(sim)
#' }

summary.do.openWARPlayers = function(object, ...) {
  object %>% dplyr::select_(~playerId, ~Name, ~WAR) %>% 
    group_by_(~playerId) %>% 
    summarise_(Name = ~Name[1], N = ~n(), q0 = ~min(WAR), 
               q2.5 = ~quantile(WAR, 0.025), 
               q25 = ~quantile(WAR, 0.25), 
               q50 = ~mean(WAR), 
               q75 = ~quantile(WAR, 0.75), 
               q97.5 = ~quantile(WAR, 0.975), 
               q100 = ~max(WAR)) %>%
    arrange_(~desc(q50)) %>% 
    tbl_df()
}




#' @title Visualize resampled openWAR
#' 
#' @description Visualize resampled openWAR
#' 
#' @details Density Plots for openWAR estimates
#' 
#' @param x A data.frame resulting from shakeWAR() of class \code{do.openWARPlayers}
#' @param playerIds A vector of valid MLBAM player IDs present in the data argument
#' @param ... arguments passed to \code{\link{densityplot}}
#' 
#' @return a faceted densityplot
#' 
#' @import lattice
#' @importFrom tidyr gather
#' @export
#' @examples
#' 
#'
#' \dontrun{
#' openWAR.sim = shakeWAR(May)
#' plot(openWAR.sim, playerIds = c(431151, 502517, 408234, 285078, 518774, 285079))
#' }

plot.do.openWARPlayers = function(x, playerIds = c(431151, 285079), ...) {
  playerIds <- sort(playerIds)
  # is it worth the trouble to filter the rows?
  rows <- filter_(x, ~playerId %in% playerIds)
  # Remove unused factor levels
  rows$Name = factor(rows$Name)
  
  lkup = unique(rows[, c("playerId", "Name")])
  lkup <- rows %>%
    select_(~playerId, ~Name) %>%
    unique()
  #    labels = as.character(lkup[order(lkup$batterId), ]$Name)
  labels = as.character(arrange_(lkup, ~playerId)$Name)
  
  sims_tidy <- rows %>%
      select_("playerId", "Name", "RAA", "RAA.bat", "RAA.br", "RAA.field", "RAA.pitch") %>%
      tidyr::gather(key = "component", value = "RAA", -playerId, -Name)
  
  plot = lattice::densityplot(~RAA | component, groups = ~playerId, data = sims_tidy
                     , panel = function(x, y, ...) { 
                       panel.densityplot(x, plot.points = FALSE, lwd = 3, ...)
                     }
                     , auto.key = list(columns = min(4, length(playerIds)), text = labels)
                     , ylim = c(-0.01, 0.2), xlab = "Runs Above Average (RAA)"
  )
  return(plot)
}


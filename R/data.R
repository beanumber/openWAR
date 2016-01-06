#' Samples of play-by-play data from MLBAM
#' 
#' Datasets containing play-by-play data from MLBAM GameDay and processed by 
#' \code{\link{openWAR}}. \code{MetsBraves} contains data for the August 
#' 8th, 2012 game between the New York Mets and Atlanta Braves. \code{May14}
#' contains data from all games on May 14, 2013. \code{May} contains data
#' for all games from May 2013. 
#' 
#' @format For \code{MetsBraves}, a \code{\link{gameday}} object. For 
#' \code{\link{May14}} and \code{\link{May}}, a \code{\link{GameDayPlays}}
#' data frame.
#' @docType data
#' 
#' @source \url{http://gd2.mlb.com/components/game/mlb/year_2012/month_08/day_12/gid_2012_08_12_atlmlb_nynmlb_1/}
#' @source \url{http://gd2.mlb.com/components/game/mlb/year_2012/month_05/day_14/}
#' @source \url{http://gd2.mlb.com/components/game/mlb/year_2012/month_05/}
"MetsBraves" 

#' @rdname MetsBraves
#' @docType data
"May14" 

#' @rdname MetsBraves
#' @docType data
"May" 

#' A sample of processed data that has been through \code{\link{makeWAR}}
#' 
#' @format A list with four components:
#' \describe{
#'    \item{plays}{the original \code{\link{GameDayPlays}} data on which the models were fit. NULL in this case}
#'    \item{data}{NULL}
#'    \item{models.used}{a list of model objects used in the computation of \code{\link{makeWAR}}}
#'    \item{openWAR}{an \code{\link{openWARPlays}} (\code{\link{data.frame}}) with the resulting processed values, based on the openWAR model}
#'    }
#' 
"MayProcessed" 

#' Final openWAR values for 2012.
#' 
#' A dataset containing computed openWAR values for MLB players during the 2012 season along with
#' the runs above average for each component of openWAR.    
#' 
#' @docType data
#' @format An \code{\link{openWARPlayers}} object, which is also a \code{\link{data.frame}} with 
#' 40 columns. Each row represents a single player. The columns are as follows:
#' \describe{
#'    \item{playerId}{the MLBAM id of the player}
#'    \item{PA.bat}{Number of plate appearances}
#'    \item{G}{Games Played}
#'    \item{HR}{Homeruns}
#'    \item{RAA.bat}{Runs above average for batting}
#'    \item{PA.br1}{Plate appearance in which this player was a baserunner on first base}
#'    \item{RAA.br1}{Runs above average for plays where the player started on first base}
#'    \item{PA.br2}{Plate appearance in which this player was a baserunner on second base}
#'    \item{RAA.br2}{Runs above average for plays where the player started on second base}
#'    \item{PA.br3}{Plate appearance in which this player was a baserunner on third base}
#'    \item{RAA.br3}{Runs above average for plays where the player started on third base}
#'    \item{BF}{Number of batters faced by a pitcher (i.e. 0 for most hitters)}
#'    \item{RAA.pitch}{Runs above average for pitching}
#'    \item{Name}{Players Name}
#'    \item{PA.P}{Number of plate appearance that the player's defensive postion was pitcher}
#'    \item{RAA.P}{Runs above average for a player when their defensive position was pitcher}
#'    \item{PA.C}{Number of plate appearance that the player's defensive postion was catcher}
#'    \item{RAA.C}{Runs above average for a player when their defensive position was catcher}
#'    \item{PA.1B}{Number of plate appearance that the player's defensive postion was first base}
#'    \item{RAA.1B}{Runs above average for a player when their defensive position was first base}
#'    \item{PA.2B}{Number of plate appearance that the player's defensive postion was second base}
#'    \item{RAA.2B}{Runs above average for a player when their defensive position was second base}
#'    \item{PA.3B}{Number of plate appearance that the player's defensive postion was third base}
#'    \item{RAA.3B}{Runs above average for a player when their defensive position was third base}
#'    \item{PA.SS}{Number of plate appearance that the player's defensive postion was shortstop}
#'    \item{RAA.SS}{Runs above average for a player when their defensive position was shortstop}
#'    \item{PA.LF}{Number of plate appearance that the player's defensive postion was left field}
#'    \item{RAA.LF}{Runs above average for a player when their defensive position was left field}  
#'    \item{PA.CF}{Number of plate appearance that the player's defensive postion was center field}
#'    \item{RAA.CF}{Runs above average for a player when their defensive position was center field}
#'    \item{PA.RF}{Number of plate appearance that the player's defensive postion was right field}
#'    \item{RAA.RF}{Runs above average for a player when their defensive position was right field}
#'    \item{RAA.br}{Total runs above average for baserunning}
#'    \item{RAA.off}{Total runs above average on offense (i.e. RAA.bat + RAA.br)}
#'    \item{RAA.field}{Total runs above average for fielding}
#'    \item{RAA}{Total runs above average from the four components (i.e RAA.bat + RAA.br + RAA.pitch + RAA.field)}
#'    \item{TPA}{Total plate appearance}
#'    \item{repl}{Replacement level for the shadow player}
#'    \item{WAR}{The players openWAR value}
#'    \item{isReplacement}{An indicator for whether the player was including in the replacement pool for calculating replacement level.  }
#'    
#'    
#'    
#'    
#'    }
#' @seealso \code{\link{getWAR}}, \code{\link{plot.openWARPlayers}}, 
#' \code{\link{summary.openWARPlayers}}
#' 
"openWAR2012"
#' @rdname openWAR2012
#' @docType data
"openWAR2013"
#' @rdname openWAR2012
#' @docType data
"openWAR2014"
#' @rdname openWAR2012
#' @docType data
"openWAR2015"

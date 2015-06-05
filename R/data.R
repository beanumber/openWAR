#' A sample of play-by-play data from MLBAM
#' 
#' A dataset containing play-by-play data from MLBAM GameDay and processed by 
#' \code{openWAR} for the August 8th, 2012 game between the Mets and Braves. 
#' 
#' @format A `gameday` object. This consists of a list of four elements:
#' \describe{
#'    \item{gameId}{the MLBAM id for the game}
#'    \item{base}{the root directory on the GameDay server for this date}
#'    \item{url}{a vector of URLs for the components of the game that will be 
#'    downloaded}
#'    \item{ds}{a \code{data.frame} with 75 rows (e.g. plays) and 62 columns. The columns are as follows:
#'      \describe{
#'        \item{pitcherId}{the MLBAM id of the pitcher}
#'        \item{batterId}{the MLBAM id of the batter}
#'      }
#'    }
#' }
#' 
#' @source \url{http://gd2.mlb.com/components/game/mlb/year_2012/month_08/day_12/gid_2012_08_12_atlmlb_nynmlb_1/}
#' 
"MetsBraves" 

#' A sample of GameDayPlays data from MLBAM
#' 
#' A dataset containing play-by-play data from MLBAM GameDay and processed by 
#' \code{openWAR} for all games on May 14, 2013. 
#' 
#' @format A `GameDayPlays` object, which is also a \code{data.frame} with 1132 
#' rows (e.g. plays) and 62 columns. The columns are as follows:
#' \describe{
#'    \item{pitcherId}{the MLBAM id of the pitcher}
#'    \item{batterId}{the MLBAM id of the batter}
#'    }
#' 
#' @source \url{http://gd2.mlb.com/components/game/mlb/year_2012/month_05/day_14/}
#' 
"May14" 

#' A sample of GameDayPlays data from MLBAM
#' 
#' A dataset containing play-by-play data from MLBAM GameDay and processed by 
#' \code{openWAR} for all games in May, 2013. 
#' 
#' @format A `GameDayPlays` object, which is also a \code{data.frame} with 31738 
#' rows (e.g. plays) and 62 columns. The columns are as follows:
#' \describe{
#'    \item{pitcherId}{the MLBAM id of the pitcher}
#'    \item{batterId}{the MLBAM id of the batter}
#'    }
#' 
#' @source \url{http://gd2.mlb.com/components/game/mlb/year_2012/month_05/}
#' 
"May" 


#' Final openWAR values for 2012.
#' 
#' A dataset containing computed openWAR values for MLB players during the 2012 season along with
#' the runs above average for each component of openWAR.    
#' 
#' @format An `openWARPlayers` object, which is also a \code{data.frame} with 1284
#' rows (e.g. a player) and 41 columns. The columns are as follows:
#' \describe{
#'    \item{playerId}{the MLBAM id of the player}
#'    \item{batterId}{the MLBAM id of the batter}
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
#' 
#' 
"openWAR2012"

#' @rdname openWAR2012
"openWAR2013"
#' @rdname openWAR2012
"openWAR2014"
#' @rdname openWAR2012
"openWAR2015"

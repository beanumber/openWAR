#' A sample of play-by-play data from MLBAM
#' 
#' A dataset containing play-by-play data from MLBAM GameDay and processed by 
#' \code{openWAR} for the August 8th, 2012 game between the New York Mets and Atlanta Braves. 
#' 
#' @format A `gameday` object. This consists of a list of four elements:
#' \describe{
#'    \item{gameId}{The MLBAM id for the game}
#'    \item{base}{The root directory on the GameDay server for this date}
#'    \item{url}{A vector of URLs for the components of the game that will be 
#'    downloaded}
#'    \item{ds}{A \code{data.frame} with 75 rows (e.g. plays) and 62 columns. The columns are as follows:
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
#'         \item{x}{For balls in play, the MLBAM x-coordinate of where the ball is first touched. (NA for balls not in play)}
#'         \item{y}{For balls in play, the MLBAM y-coordinate of where the ball is first touched. (NA for balls not in play)}
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
#'         \item{our.x}{}
#'         \item{out.y}{}
#'         \item{r}{}
#'         \item{theta}{}
#'         
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
#'         \item{pitcherId}{The MLBAM id of the pitcher}
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
#'         \item{x}{For balls in play, the MLBAM x-coordinate of where the ball is first touched. (NA for balls not in play)}
#'         \item{y}{For balls in play, the MLBAM y-coordinate of where the ball is first touched. (NA for balls not in play)}
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
#'         \item{our.x}{}
#'         \item{out.y}{}
#'         \item{r}{}
#'         \item{theta}{}
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
#'     \item{pitcherId}{The MLBAM id of the pitcher}
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
#'         \item{x}{For balls in play, the MLBAM x-coordinate of where the ball is first touched. (NA for balls not in play)}
#'         \item{y}{For balls in play, the MLBAM y-coordinate of where the ball is first touched. (NA for balls not in play)}
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
#'         \item{our.x}{}
#'         \item{out.y}{}
#'         \item{r}{}
#'         \item{theta}{}
#'    }
#' 
#' @source \url{http://gd2.mlb.com/components/game/mlb/year_2012/month_05/}
#' 
"May" 

#' A sample of processed data that has been through \code{makeWAR}
#' 
#' @format A list with four components:
#' \describe{
#'    \item{plays}{the original \code{GameDayPlays} data on which the models were fit. NULL in this case}
#'    \item{data}{NULL}
#'    \item{models.used}{a list of model objects used in the computation of \code{makeWAR}}
#'    \item{openWAR}{a data.frame with the resulting processed values, based on the openWAR model}
#'    }
#' 
"MayProcessed" 

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

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

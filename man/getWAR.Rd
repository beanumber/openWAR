\name{getWAR}
\alias{getWAR}
\title{getWAR}
\usage{
  getWAR(data, ...)

  getWAR(data, ...)
}
\arguments{
  \item{con}{An RMySQL database connection}

  \item{ids}{A character vector of player ids that will be
  retrieved}

  \item{start}{A numeric representing the start year of the
  interval for selecting WAR}

  \item{end}{A numeric representing the end year of the
  interval for selecting WAR}

  \item{data}{An MLBAM data.frame}
}
\value{
  A data.frame with the following elements: \item{PlayerID
  }{Character string containnig the player id} \item{yearID
  }{Year} \item{stintId }{???} \item{teamID }{The team that
  the player played on} \item{lgId }{The league the player
  played in (National or American)} \item{R_BAT }{Batting
  component} \item{R_BR }{Running component} \item{R_DP
  }{Double Play component} \item{R_FIELD }{Fielding
  component} \item{R_POS }{Position component} \item{R_REPL
  }{Replacemnt player value} \item{RAA }{Runs above
  average} \item{RAR }{Runs above replacement} \item{WAR
  }{Wins above replacement}

  a data.frame of RAA values per player
}
\description{
  Retrieves WAR from a database table

  Retrieve openWAR
}
\details{
  This function will retrieve bWAR data for all players
  within a specified timeframe.

  Retrieves openWAR, aggregated by player, given an MLBAM
  data set
}
\examples{
# Create a connection to the database
con = getCon()
# Pull bWAR for 2012
war = getWAR(con)
# Pull bWAR for the 1980s
war = getWAR(con, start=1980, end=1989)
ds = getData()
res = getWAR(ds)
}


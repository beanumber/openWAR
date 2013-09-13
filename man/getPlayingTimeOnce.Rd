\name{getPlayingTimeOnce}
\alias{getPlayingTimeOnce}
\title{getPlayingTime}
\usage{
  getPlayingTimeOnce(data, playerIds)
}
\arguments{
  \item{data}{An openWARPlays data.frame}

  \item{playerIds}{A vector of playerIds}
}
\value{
  a matrix of playing time counts for each player at each
  position
}
\description{
  Identify the playing time for all players
}
\details{
  Returns a matrix of playing time at each activity for
  each player
}
\examples{
data = getData()
ds = makeWAR(data)
players = getWAR(ds$openWAR)
summary(players)
}


\name{getReplacementPlayers}
\alias{getReplacementPlayers}
\title{getReplacementPlayers}
\usage{
  getReplacementPlayers(data)
}
\arguments{
  \item{data}{An object of class \code{"RAA"}}
}
\description{
  Identifies the replacement level players
}
\details{
  Returns a list of playerIds, who constitute
  replacement-level players
}
\examples{
ds = getData()
out = makeWAR(ds)
players = getWAR(out)
summary(out)
replacementIds = getReplacementPlayers(players)
}


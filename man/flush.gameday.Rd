\name{flush.gameday}
\alias{flush.gameday}
\title{flush.gameday}
\usage{
\method{flush}{gameday}(x)
}
\arguments{
  \item{gd}{A gameday object}

  \item{gameId}{A valid MLBAM gameId}
}
\description{
Remove a troublesome gameday cache
}
\details{
Given a gameday object or gameId, delete all local cache
files
}
\examples{
gd = gameday()
getURLs.gameday(gd)
}


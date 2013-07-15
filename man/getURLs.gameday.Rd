\name{getURLs.gameday}
\alias{getURLs.gameday}
\alias{getXML.gameday}
\title{getURLs.gameday}
\usage{
  getURLs.gameday(gd)
}
\arguments{
  \item{gameId}{A valid MLBAM gameId}

  \item{type}{The type of file}
}
\value{
  a URL, or an XML file
}
\description{
  Convenience function for returning data related to a game
}
\details{
  Given a gameId string, this function can return several
  URLs
}
\examples{
gd = gameday()
getURLs.gameday(gd)
}


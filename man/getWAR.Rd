\name{getWAR}
\alias{getWAR}
\title{getWAR}
\usage{
  getWAR(data, ...)
}
\arguments{
  \item{data}{An openWARPlays data.frame}
}
\value{
  an RAA object
}
\description{
  Tabulates RAA values per player
}
\details{
  takes an openWARPlays data.frame and consolidates the RAA
  values by player
}
\examples{
data = getData()
ds = makeWAR(data)
players = getWAR(ds$openWAR)
summary(players)
}


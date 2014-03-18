\name{getRAA}
\alias{getRAA}
\title{getRAA}
\usage{
getRAA(data, ...)
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
players = getRAA(ds$openWAR)
summary(players)
}


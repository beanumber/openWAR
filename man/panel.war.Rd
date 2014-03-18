\name{panel.war}
\alias{panel.war}
\title{panel.war}
\usage{
panel.war(x, y, ...)
}
\arguments{
  \item{x}{}

  \item{y}{}

  \item{...}{arguments passed from
  \code{"plot.openWARPlayers"}}
}
\description{
Display a season's worth of openWAR results
}
\details{
Given an openWARPlayers object, draw a plot displaying each
player's RAA, WAR, and replacement level shadow.
}
\examples{
ds = getData()
out = makeWAR(ds)
players = getWAR(out$openWAR)
summary(players)
plot(players)
}


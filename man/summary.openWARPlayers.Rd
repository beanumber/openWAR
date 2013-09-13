\name{summary.openWARPlayers}
\alias{summary.openWARPlayers}
\title{summary.openWARPlayers}
\usage{
  summary.openWARPlayers(data, n = 25, ...)
}
\arguments{
  \item{data}{An object of class \code{"openWAR"}}
}
\description{
  Summarize WAR among players
}
\details{
  A summary of players' WAR
}
\examples{
ds = getData()
out = (makeWAR(ds))
summary(out)
}


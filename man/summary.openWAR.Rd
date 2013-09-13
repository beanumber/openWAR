\name{summary.openWAR}
\alias{summary.openWAR}
\title{summary.openWAR}
\usage{
  summary.openWAR(data, n = 25, ...)
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


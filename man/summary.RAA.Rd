\name{summary.RAA}
\alias{summary.RAA}
\title{summary.RAA}
\usage{
  summary.RAA(data, ...)
}
\arguments{
  \item{data}{An object of class \code{"RAA"}}
}
\description{
  Summarize Runs Above Average among players
}
\details{
  A Summary of players' WAR
}
\examples{
ds = getData()
out = (makeWAR(ds))
summary(out)
}


\name{summary.do.RAA}
\alias{summary.do.RAA}
\title{summary}
\usage{
  summary.do.RAA(data, n = 25, ...)
}
\arguments{
  \item{An}{object of class \code{"WAR"}}
}
\description{
  Summarize WAR
}
\details{
  Summary of players' WAR
}
\examples{
ds = makeWAR(ds)
sim = shakeWAR(ds)
summary(sim)
}


\name{summary.war}
\alias{summary.war}
\title{summary}
\usage{
  summary.war(data, ...)
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
ds = getData()
out = (makeWAR(ds))
summary(out)
}


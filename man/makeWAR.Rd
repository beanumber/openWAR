\name{makeWAR}
\alias{makeWAR}
\alias{makeWAR.GameDayPlays}
\title{makeWAR}
\usage{
  makeWAR(data, type, drop.incomplete = TRUE, ...)
}
\arguments{
  \item{data}{A GameDayPlays data set}

  \item{re.mod}{A run expectancy model with a predict()
  method. Expects to take the two variables startCode and
  startOuts}

  \item{verbose}{A LOGICAL indicating whether you want
  various messages and information to be displayed during
  the computation}
}
\value{
  a data.frame
}
\description{
  Compute openWAR
}
\details{
  Computes openWAR, given an MLBAM data set
}
\examples{
ds = getData()
res = makeWAR(ds)
}


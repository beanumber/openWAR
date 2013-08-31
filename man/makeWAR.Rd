\name{makeWAR}
\alias{makeWAR}
\alias{makeWAR.GameDayPlays}
\title{makeWAR}
\usage{
  makeWAR(data, models = list(), verbose = TRUE, ...)
}
\arguments{
  \item{data}{A GameDayPlays data set}

  \item{models}{A named list of models, each with a
  predict() method. See Details.}

  \item{verbose}{A LOGICAL indicating whether you want
  various messages and information to be displayed during
  the computation}
}
\value{
  a GameDayPlaysExt data.frame
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


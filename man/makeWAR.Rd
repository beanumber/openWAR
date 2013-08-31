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
  Computes openWAR, given an MLBAM data set. If no
  \code{models} argument is supplied, then all models
  necessary for the computation of openWAR will be
  generated on the data set given.

  If \code{verbose == TRUE}, then various pieces of
  information will be displayed during the comuptation

  Elements of \code{models}: run-expectancy: a model for
  assigning a run expectancy value to any of the 24
  (base,out) states. Variables must be "startCode" [0-7]
  and "startOuts" [0-2] pitching: a model for the expected
  outcome of a plate appearance attributable to the
  pitcher. Variables must be "stadium", "throws" [L/R], and
  "stands" [L/R] offense: a model for the expected outcome
  of a plate appearance attributable to the offense.
  Variables must be "stadium", "throws" [L/R], and "stands"
  [L/R] baserunning: a model for the expected contribution
  of the baserunners to a plate appearance. Variables must
  be "event" (the type of batting event), "startCode"
  [0-7], and "startOuts" [0-2] batting: a model for the
  expected contribution of the batter to a plate
  appearance. Variables must be "batterPos" (the defensive
  position of the batter)
}
\examples{
ds = getData()
res = makeWAR(ds)
}


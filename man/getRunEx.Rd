\name{getRunEx}
\alias{getRunEx}
\title{getRunEx}
\usage{
getRunEx(data, drop.incomplete = TRUE, ...)
}
\arguments{
  \item{data}{an MLBAM dataset}

  \item{drop.incomplete}{a LOGICAL indicating whether
  incomplete innings (e.g. walk-off innings) should be
  excluded from the run expectancy model}
}
\value{
A function that takes two arguments: baseCode and outs, and
return a run value
}
\description{
Find the Run Expectancy for a given state
}
\details{
Given a data set, return a function giving the run
expectancy for any base/out state
}
\examples{
ds = data(MLBAM2013)
fit.rem = getRunEx(ds)

fit.rem(0,0)
fit.rem(4,1)
}


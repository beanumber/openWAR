\name{getModel.GameDayPlays}
\alias{getModel.GameDayPlays}
\title{getModel.GameDayPlays}
\usage{
  getModel.GameDayPlays(data, type, drop.incomplete = TRUE,
    ...)
}
\arguments{
  \item{data}{a GameDayPlays dataset}

  \item{type}{the type of model to be built.}

  \item{drop.incomplete}{a LOGICAL indicating whether
  incomplete innings (e.g. walk-off innings) should be
  excluded}
}
\value{
  A model object with a predict() method
}
\description{
  Retrieve various models trained on GameDayPlays data
}
\details{
  This function will build various models based on the
  MLBAM data set and the openWAR framework.
}
\examples{
ds = data(MLBAM2013)
re.mod = getModel(ds, type = "run-expectancy")


predict(re.mod, newdata=)
re.mod(0,0)
fit.rem(4,1)
}


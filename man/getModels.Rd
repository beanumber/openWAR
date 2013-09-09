\name{getModels}
\alias{getModels}
\alias{getModels.GameDayPlays}
\title{getModels}
\usage{
  getModels(data, ...)
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
data(MLBAM2013)
re.mod = getModel(MLBAM2013, type = "run-expectancy")

# Generate the Run Expectancy Matrix
states = expand.grid(startCode = 0:7, startOuts = 0:2)
matrix(predict(re.mod, newdata=states), ncol=3)

begin.states = MLBAM2013[,c("startCode", "startOuts")]
ds = transform(MLBAM2013, startEx = predict(re.mod, newdata=begin.states))
end.states = MLBAM2013[,c("endCode", "endOuts")]
end.states$endOuts = with(end.states, ifelse(endOuts == 3, NA, endOuts))
names(end.states) = names(begin.states)
ds = transform(ds, endEx = predict(re.mod, newdata=end.states))
ds$endEx = with(ds, ifelse(is.na(endEx), 0, endEx))
}


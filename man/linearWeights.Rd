\name{linearWeights}
\alias{linearWeights}
\title{linearWeights}
\usage{
  linearWeights(data, type = "xr", ...)
}
\arguments{
  \item{data}{An MLBAM data set}

  \item{type}{A string representing either "xr27", "woba"}
}
\value{
  A numeric vector of linear weights values
}
\description{
  Apply a linear weights formula to an MLBAM data set
}
\details{
  Applies any of several linear weights formulae to an
  MLBAM data set
}
\examples{
ds = getData()
ds$woba = linearWeights(ds, type = "woba")
ds$xr = linearWeights(ds, type = "xr")
leaders = ddply(ds, ~batterId, summarise, Name = batterName[1], PA = sum(isPA)
, WOBA = sum(woba) / sum(isPA == TRUE), XR27 = sum(xr) * 25.5 / sum(isPA & !isHit))
# The top 20
head(leaders[order(leaders$WOBA, decreasing = TRUE),], 20)
# meeting a plate appearance minimum
qualified = subset(leaders, PA > length(unique(ds$gameId)) * 3.1 / 15)
# WOBA leaders
head(qualified[order(qualified$WOBA, decreasing = TRUE),], 20)
# XR27 leaders
head(qualified[order(qualified$XR27, decreasing = TRUE),], 20)
# Compare the two metrics
xyplot(XR27 ~ WOBA, data = qualified)
with(qualified, cor(WOBA, XR27))
}


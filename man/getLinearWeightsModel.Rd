\name{getLinearWeightsModel}
\alias{getLinearWeightsModel}
\title{getLinearWeightsModel}
\usage{
  getLinearWeightsModel(type)
}
\arguments{
  \item{type}{A string representing either "xr27", "woba"}
}
\value{
  A \code{lm} object
}
\description{
  Retrieves a linear weights model object
}
\details{
  Retrieves any of several linear weights formulae as
  \code{lm} objects
}
\examples{
ds = getData()
event = data.frame(event = ds$event)
woba = getLinearWeightsModel(type = "woba")
xr = getLinearWeightsModel(type = "xr")
ds$woba = predict(woba, newdata=event)
ds$xr = predict(xr, newdata=event)
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


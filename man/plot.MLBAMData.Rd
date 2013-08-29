\name{plot.MLBAMData}
\alias{plot.MLBAMData}
\title{plot.MLBAMData}
\usage{
  plot.MLBAMData(data, ...)
}
\arguments{
  \item{data}{An MLBAMData set with fields "our.x" and
  "our.y"}
}
\value{
  an xyplot()
}
\description{
  Visualize Balls in Play
}
\details{
  Plots the balls in play from GameDay data. This function
  will plot (x,y)-coordinates with a generic baseball field
  plotted in the background. Other lattice options can be
  passed to xyplot().
}
\examples{
ds = getData()
plot(ds)
}


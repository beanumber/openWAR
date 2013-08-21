\name{summary}
\alias{summary}
\title{bbplot}
\arguments{
  \item{data}{An MLBAM data set with fields "our.x" and
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
bbplot(ds)
}


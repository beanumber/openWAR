\name{getLinearWeights}
\alias{getLinearWeights}
\title{getLinearWeights}
\usage{
  getLinearWeights(con, start = 1980, end = 2012)
}
\arguments{
  \item{con}{An RMySQL database connection}

  \item{start}{A numeric representing the start year of the
  interval for selecting WAR}

  \item{end}{A numeric representing the end year of the
  interval for selecting WAR}
}
\value{
  A vector of weights \item{1B }{Singles} \item{2B
  }{Doubles} \item{3B }{Triples} \item{HR }{Home Runs}
  \item{BB }{Walks} \item{HBP }{Hit By Pitch} \item{SO
  }{Strikeouts} \item{GO }{Ground outs} \item{AO }{Air
  outs} \item{LO }{Line outs} \item{UO }{Pop-up outs}
  \item{Other }{Everything else -- should be 0}
}
\description{
  Computes Linear Weights estimates
}
\details{
  Pull data over a certain time period, and run MLR model
  to find Linear Weights estimates
}
\examples{
# Create a connection to the database
con = getCon()
# Pull bWAR for 2012
war = getWAR(con)
# Pull bWAR for the 1980s
war = getWAR(con, start=1980, end=1989)
}


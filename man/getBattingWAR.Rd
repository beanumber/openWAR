\name{getBattingWAR}
\alias{getBattingWAR}
\title{getWARbatting}
\usage{
  getBattingWAR(playerId, start = 1980, end = 2012,
    nSim = 10)
}
\arguments{
  \item{playerId,}{start, end, nSim}
}
\value{
  A list with two elements \item{WARbatting}{Value of
  batting WAR} \item{boots}{nSim simulated values of
  batting WAR}
}
\description{
  Calculates the batting component of WAR and calculates
  the standard deviation via simulation.
}
\details{
  Fill in details
}
\examples{
#Create a connection to the database
con<-getCon()
#Calculate Bobby Abreu batting WAR component from 2001 to 2005
WARbatting<-getWARbatting("abreb001",start=2001,end=2005,nSim=1000)
}


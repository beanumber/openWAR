\name{offWarBoot}
\alias{offWarBoot}
\title{offWarBoot}
\usage{
  offWarBoot(offStatsVec, nSim = 10)
}
\arguments{
  \item{off.stats.vec}{}
}
\value{
  A vector with two elements \item{offWar}{Values of
  offenwsive WAR} \item{St.Dev.}{Standard deviation of WAR
  calculated via simulation}
}
\description{
  Calculates the offensive component of WAR and calculates
  the standard deviation via simulation.
}
\details{
  Input a vector of offensive stats as in the output from
  the function getBatting
}
\examples{
#Create a connection to the database
con<-getCon()
#Get weights'
wts<-getLinearWeights(con,start=1980,end=2012)
#Get batting stats
bats<-getBatting(con,start=2000,end=2012)
#Pull out Bobby Abreu
dat<-bats[bats$batterId=="abreb001",]
#Calculate offensive WAR for each year with a bootstrapped estimate of variance
wars<-apply(dat,1,offWarBoot,nSim=100)
results<-cbind(dat,t(wars))
#sort the results
results[order(-results$offWar),]
}


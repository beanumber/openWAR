#' @title getWARbatting
#' 
#' @description Calculates the batting component of WAR and calculates the standard deviation via simulation.
#' 
#' @details Fill in details
#' 
#' @param playerId, start, end, nSim
#' 
#' @return A list with two elements
#' \item{WARbatting}{Value of batting WAR}
#' \item{boots}{nSim simulated values of batting WAR}
#' 
#' @export
#' @examples
#' #Create a connection to the database
#' con<-getCon()
#' #Calculate Bobby Abreu batting WAR component from 2001 to 2005
#' WARbatting<-getWARbatting("abreb001",start=2001,end=2005,nSim=1000)

getBattingWAR<-function(playerId,start=1980,end=2012,nSim=10){
  #Get data
  batting<-getBatting(con,start,end)
  #Pull out only the playerIds of interest
  dat<-batting[batting$batterId==playerId,]
  rm(batting)
  wts<-getLinearWeights(con,start,end)
  battingStatsVec<-apply(dat[3:14],2,sum)
  WARbatting<-battingStatsVec[-1]%*%wts
  #For variance
  boots<-rep(NA,nSim)
  TPA<-battingStatsVec['TPA']  
  for (i in 1:nSim){
    if (i%%1000==0){print(i)}
    smp<-factor(sample(c(1:11),TPA,prob=battingStatsVec[-1]/TPA,replace=TRUE),levels=c(1:11))
    table(smp)
    boots[i]<-(table(smp)%*%wts)
      }
return(list(WARbatting=WARbatting,boots=boots))
}



#' @title makeWAR
#' 
#' @description Compute openWAR
#' 
#' @details Computes openWAR, given an MLBAM data set
#' 
#' @param data An MLBAM data.frame 
#' 
#' @return a data.frame
#' 
#' @export
#' @examples
#' 
#' ds = getData()
#' res = makeWAR(ds)
#' 

setClass("WAR",representation(madeWAR="data.frame",playerWAR="data.frame"))
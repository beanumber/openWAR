#' @title WAR-class
#' 
#' @description Contains the output from makeWAR
#' 
#' 
#' 
#' 
#' 
#' @section Slots:
#'    \describe{
#'    \item{\code{madeWAR}:}{Object of class \code{"data.frame"}.}
#'    \item{\code{playerWAR}:}{Object of class \code{"data.frame"}.}
#'}
#'
#' 
#' 
#' 
#' 
#'
#' @examples showClass("WAR")
#' 
#' 
#' 
#' 

setClass("WAR",representation(madeWAR="data.frame",playerWAR="data.frame"))
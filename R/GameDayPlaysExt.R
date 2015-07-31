#' @title A processed \code{GameDayPlays} data set.
#' 
#' @description Contains the output from \code{\link{makeWAR}}.
#' 
#' @exportClass GameDayPlaysExt
#' @examples showClass('GameDayPlaysExt')

setClass("GameDayPlaysExt", contains = "GameDayPlays") 

#' @title RAA
#' 
#' @description Contains the output from getWAR()
#' 
#' @exportClass RAA
#' @examples showClass("RAA")

setClass("RAA", contains = "data.frame")

#' @title summary.RAA
#' 
#' @description Summarize Runs Above Average among players
#' 
#' @details A Summary of players' WAR
#' 
#' @param data An object of class \code{"RAA"}
#' 
#' @export summary.RAA
#' @examples
#' 
#' ds = getData()
#' out = (makeWAR(ds))
#' summary(out)

summary.RAA = function (data, n = 25, ...) {
  cat(paste("Displaying information for", nrow(data), "players, of whom", nrow(subset(data, RAA.pitch != 0)), "have pitched\n"))
  head(data[order(data$RAA, decreasing=TRUE), c("Name", "TPA", "RAA", "RAA.bat", "RAA.br", "RAA.field", "RAA.pitch")], n)
}

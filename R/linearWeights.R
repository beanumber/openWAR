#' @title linearWeights
#' 
#' @description Apply a linear weights formula to an MLBAM data set
#' 
#' @details Applies any of several linear weights formulae to an MLBAM data set
#' 
#' @param data An MLBAM data set
#' @param type A string representing either "xr27", "woba"
#'
#' @return A numeric vector of linear weights values
#' 
#' @export
#' @examples
#' 
#' ds = getData()
#' ds$woba = linearWeights(ds, type = "woba")
#' ds$xr = linearWeights(ds, type = "xr")
#' leaders = ddply(ds, ~batterId, summarise, Name = batterName[1], PA = sum(isPA)
#' , WOBA = sum(woba) / sum(isPA == TRUE), XR27 = sum(xr) * 25.5 / sum(isPA & !isHit))
#' # The top 20
#' head(leaders[order(leaders$WOBA, decreasing = TRUE),], 20)
#' # meeting a plate appearance minimum
#' qualified = subset(leaders, PA > length(unique(ds$gameId)) * 3.1 / 15)
#' # WOBA leaders
#' head(qualified[order(qualified$WOBA, decreasing = TRUE),], 20)
#' # XR27 leaders
#' head(qualified[order(qualified$XR27, decreasing = TRUE),], 20)
#' # Compare the two metrics
#' xyplot(XR27 ~ WOBA, data = qualified)
#' with(qualified, cor(WOBA, XR27))

linearWeights = function (data, type = "xr", ...) {
  lw = sapply(data$event, getLinearWeight, type = type)
  return(lw)
}

getLinearWeight = function (event, type = "xr", ...) {
  if (type == "woba") {
    if (event == "Home Run") {
      wgt = 1.95
    } else if (event == "Triple") {
      wgt = 1.56
    } else if (event == "Double") {
      wgt = 1.24
    } else if (event == "Single") {
      wgt = 0.9
    } else if (event == "Hit By Pitch") {
      wgt = 0.75
    } else if (event == "Walk") {
      wgt = 0.72
    } else if (event %in% c("Field Error", "Error")) {
      wgt = 0.92
    } else {
      wgt = 0
    }
  } else { # XR27
    if (event == "Home Run") {
      wgt = 1.44
    } else if (event == "Triple") {
      wgt = 1.04
    } else if (event == "Double") {
      wgt = 0.72
    } else if (event == "Single") {
      wgt = 0.5
    } else if (event %in% c("Hit By Pitch", "Walk")) {
      wgt = 0.34
    } else if (event == "Intent Walk") {
      wgt = 0.25
    } else if (event == "Grounded Into DP") {
      wgt = -0.37
    } else if (event == "Sac Fly") {
      wgt = 0.37
    } else if (event == "Sac Bunt") {
      wgt = 0.04
    } else if (event %in% c("Strikeout", "Strikeout - DP")) {
      wgt = -0.098
    } else if (event %in% c("Bunt Groundout", "Bunt Lineout", "Bunt Pop Out"
                            , "Error", "Field Error", "Fielders Choice", "Fielders Choice Out"
                            , "Flyout", "Forceout", "Groundout", "Lineout", "Pop Out"
                            , "Sac Fly DP", "Sacrifice Bunt DP", "Triple Play")) {
      wgt = -0.09
    } else {
      wgt = 0
    }
  }
  return(wgt)
}


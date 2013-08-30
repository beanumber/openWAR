#' @title getLinearWeightsModel
#' 
#' @description Retrieves a linear weights model object
#' 
#' @details Retrieves any of several linear weights formulae as \code{lm} objects
#' 
#' @param type A string representing either "xr27", "woba"
#'
#' @return A \code{lm} object
#' 
#' @export
#' @examples
#' 
#' ds = getData()
#' event = data.frame(event = ds$event)
#' woba = getLinearWeightsModel(type = "woba")
#' xr = getLinearWeightsModel(type = "xr")
#' ds$woba = predict(woba, newdata=event)
#' ds$xr = predict(xr, newdata=event)
#' 
#' braa = getLinearWeightsModelfromData(ds, type = "bp")
#' bp.events = levels(as.factor(braa$model$event))
#' event[which(!(event %in% bp.events))] <- NA
#' ds$braa = predict(braa, newdata=event)
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

getLinearWeightsModel = function (type) {
  all.events = c("Balk","Batter Interference","Batter Turn","Bunt Groundout","Bunt Lineout"
                 ,"Bunt Pop Out","Catcher Interference","Caught Stealing 2B","Caught Stealing 3B"
                 ,"Caught Stealing Home","Defensive Indiff","Defensive Sub","Defensive Switch","Double"
                 ,"Double Play","Ejection","Error","Fan interference","Field Error","Fielders Choice"
                 ,"Fielders Choice Out","Flyout","Forceout","Game Advisory","Grounded Into DP"
                 ,"Groundout","Hit By Pitch","Home Run","Intent Walk","Lineout","Offensive sub"
                 ,"Passed Ball","Picked off stealing 2B","Picked off stealing 3B","Picked off stealing home"
                 ,"Pickoff 1B","Pickoff 2B ","Pickoff 3B","Pickoff Error 1B","Pickoff Error 2B"
                 ,"Pickoff Error 3B","Pitching Substitution","Player Injured","Pop Out","Runner Advance"
                 ,"Runner Out","Sac Bunt","Sac Fly","Sac Fly DP","Sacrifice Bunt DP","Single","Stolen Base 2B"
                 ,"Stolen Base 3B","Strikeout","Strikeout - DP","Triple","Triple Play","Umpire Substitution"
                 ,"Walk","Wild Pitch" )
  if (type == "xr") {
    event = c("Home Run", "Triple", "Double", "Single", "Walk", "Hit By Pitch", "Intent Walk"
               , "Grounded Into DP", "Sac Fly", "Sac Bunt", "Strikeout", "Strikeout - DP")
    bip.outs = c("Bunt Groundout", "Bunt Lineout", "Bunt Pop Out"
                 , "Error", "Field Error", "Fielders Choice", "Fielders Choice Out"
                 , "Flyout", "Forceout", "Groundout", "Lineout", "Pop Out"
                 , "Sac Fly DP", "Sacrifice Bunt DP", "Triple Play")
    xr.events = c(event, bip.outs)
    leftover = setdiff(all.events, xr.events)
    val = c(1.44, 1.04, 0.72, 0.5, 0.34, 0.34, 0.25, -0.37, 0.37, 0.04, -0.098, -0.098, rep(-0.09, length(bip.outs)))
    data = data.frame(val = c(val, rep(0, length(leftover))), event = c(xr.events, leftover))
  }
  if (type == "woba") {
    woba.events = c("Home Run", "Triple", "Double", "Single", "Walk", "Hit By Pitch", "Field Error", "Error")
    leftover = setdiff(all.events, woba.events)
    val = c(1.95, 1.56, 1.24, 0.9, 0.75, 0.72, 0.92, 0.92)
    data = data.frame(val = c(val, rep(0, length(leftover))), event = c(woba.events, leftover))
  }
  mod = lm(val ~ 0 + event, data=data)
  return(mod)
}

getLinearWeightsModelfromData = function (data, type = "bp") {
  if (type == "bp") {
    bp.events = c("Home Run", "Triple", "Double", "Single", "Walk", "Intent Walk", "Hit By Pitch", "Strikeout")
    data$event = with(data, ifelse(event %in% bp.events, as.character(event), "Out"))
  }
  mod = lm(delta ~ 0 + event, data=data)
  return(mod)
}

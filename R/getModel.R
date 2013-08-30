#' @title getModel
#' @aliases getModel.GameDayPlays
#' 
#' @description Retrieve various models trained on GameDayPlays data
#' 
#' @details This function will build various models based on the MLBAM data set and the openWAR framework.
#' 
#' @param data a GameDayPlays dataset
#' @param type the type of model to be built. 
#' @param drop.incomplete a LOGICAL indicating whether incomplete innings (e.g. walk-off innings)
#' should be excluded
#' 
#' @return A model object with a predict() method 
#' 
#' @export getModel
#' @export getModel.GameDayPlays
#' @examples
#' 
#' data(MLBAM2013)
#' re.mod = getModel(MLBAM2013, type = "run-expectancy")
#' 
#' # Generate the Run Expectancy Matrix
#' states = expand.grid(startCode = 0:7, startOuts = 0:2)
#' matrix(predict(re.mod, newdata=states), ncol=3)
#' 
#' begin.states = MLBAM2013[,c("startCode", "startOuts")]
#' ds = transform(MLBAM2013, startEx = predict(re.mod, newdata=begin.states))
#' end.states = MLBAM2013[,c("endCode", "endOuts")]
#' end.states$endOuts = with(end.states, ifelse(endOuts == 3, NA, endOuts))
#' names(end.states) = names(begin.states)
#' ds = transform(ds, endEx = predict(re.mod, newdata=end.states))
#' ds$endEx = with(ds, ifelse(is.na(endEx), 0, endEx))
#' 
#' 

getModel = function (data, type, drop.incomplete = TRUE, ...) UseMethod("getModel")

getModel.GameDayPlays = function (data, type, drop.incomplete = TRUE, ...) {
  require(mosaic)
  
  # Drop incomplete innings
  if (type == "run-expectancy" & drop.incomplete) {
    ds <- subset(data, outsInInning == 3)
  } else {
    ds <- data
  }
    
  if (type == "run-expectancy") {
    mod = lm(runsFuture ~ as.factor(startCode) * as.factor(startOuts), data=ds)
  }
  
  return(mod)
}



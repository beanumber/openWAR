#' @title getModels
#' @aliases getModels.GameDayPlays
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
#' @export getModels
#' @export getModels.GameDayPlays
#' @export getModelRunExpectancy
#' @export getModelRunExpectancy.GameDayPlays
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

getModels = function (data,...) UseMethod("getModels")

getModels.GameDayPlays = function (data, ...) {
  models = list()
  models[["run-expectancy"]] = getModelRunExpectancy(data)
  
  return(models)
}

getModelRunExpectancy = function (data, mod.re = NULL, verbose=TRUE, drop.incomplete = TRUE, ...) UseMethod("getModelRunExpectancy")

getModelRunExpectancy.GameDayPlays = function (data, mod.re = NULL, verbose=TRUE, drop.incomplete = TRUE, ...) {
  # Check to see whether the supplied run expectancy model has a predict() method
  if (!paste("predict", class(mod.re), sep=".") %in% methods(predict)) {
    message("....Supplied Run Expectancy model does not have a predict method...")
    message("....Building in-sample Run Expectancy Model...")
    # Drop incomplete innings
    if (drop.incomplete) {
      ds <- subset(data, outsInInning == 3)
    } else {
      ds <- data
    }
    # use model=FALSE option to decrease memory footprint
    # Note that qr=TRUE is necessary to use predict() later
    mod.re = lm(runsFuture ~ as.factor(startCode) * as.factor(startOuts), data=ds, model=FALSE)
  }
  
  if (verbose) {
    message("....Run Expectancy Model....")
    states = expand.grid(startCode = 0:7, startOuts = 0:2)
    print(matrix(predict(mod.re, newdata=states), ncol=3))
  }
  return(mod.re)
}

getModelPitching = function (data, mod.pitch = NULL, verbose = TRUE) {  
  if (!paste("predict", class(mod.pitch), sep=".") %in% methods(predict)) {
    message("....Supplied Pitching model does not have a predict method...")
    message("....Building in-sample Pitching Model...")  
    mod.pitch = lm(delta.pitch ~ factor(venueId) + (throws == stand), data = data[,c("delta.pitch", "venueId", "throws", "stand")])
  } 
  return(mod.pitch)
}

getModelOffense = function (data, mod.off = NULL, verbose = TRUE) {
  # Control for circumstances
  if (!paste("predict", class(mod.off), sep=".") %in% methods(predict)) {
    message("....Supplied Offense model does not have a predict method...")
    message("....Building in-sample Offense Model...")
    mod.off = lm(delta ~ factor(venueId) + (throws == stand), data = data[,c("delta", "venueId", "throws", "stand")])
  } 
  return(mod.off)
}

getModelBaserunning = function (data, mod.br = NULL, verbose = TRUE) {  
  # Siphon off the portion attributable to the baserunners   
  if (!paste("predict", class(mod.br), sep=".") %in% methods(predict)) {
    message("....Supplied Baserunning model does not have a predict method...")
    message("....Building in-sample Baserunning Model...")
    mod.br = lm(delta.off ~ event * as.factor(startCode) * as.factor(startOuts), data=data[, c("delta.off", "event", "startCode", "startOuts")])
  }
  return(mod.br)
}

getModelBatting = function (data, mod.bat = NULL, verbose = TRUE) {
  if (!paste("predict", class(mod.bat), sep=".") %in% methods(predict)) {
    message("....Supplied Batting model does not have a predict method...")
    message("....Building in-sample Batting Model...")
    mod.bat = lm(delta.bat ~ as.factor(batterPos), data = data[, c("delta.bat", "batterPos")])
  }
  return(mod.bat)
}

getModelFieldingPosition = function (data, position) {
  mod = glm((fielderPos == position) ~ poly(our.x, 2) + poly(our.y, 2) + I(our.x * our.y), data=data, family="binomial")
  return(mod)
}

getModelFieldingPitcher = function (data) {
  mod = glm((fielderPos == "P") ~ poly(our.x, 2) + poly(our.y, 2) + I(our.x * our.y), data=data, family="binomial")
  return(mod)
}

getModelFieldingCatcher = function (data) {
  mod = glm((fielderPos == "C") ~ poly(our.x, 2) + poly(our.y, 2) + I(our.x * our.y), data=data, family="binomial")
  return(mod)
}

getModelFielding1B = function (data) {
  mod = glm((fielderPos == "1B") ~ poly(our.x, 2) + poly(our.y, 2), data=data, family="binomial")
  return(mod)
}

getModelFielding2B = function (data) {
  mod = glm((fielderPos == "2B") ~ poly(our.x, 2) + poly(our.y, 2) + I(our.x * our.y), data=data, family="binomial")
  return(mod)
}

getModelFielding3B = function (data) {
  mod = glm((fielderPos == "3B") ~ poly(our.x, 2) + poly(our.y, 2) + I(our.x * our.y), data=data, family="binomial")
  return(mod)
}

getModelFieldingSS = function (data) {
  mod = glm((fielderPos == "SS") ~ poly(our.x, 2) + poly(our.y, 2) + I(our.x * our.y), data=data, family="binomial")
  return(mod)
}

getModelFieldingLF = function (data) {
  mod = glm((fielderPos == "LF") ~ poly(our.x, 2) + poly(our.y, 2) + I(our.x * our.y), data=data, family="binomial")
  return(mod)
}

getModelFieldingCF = function (data) {
  mod = glm((fielderPos == "CF") ~ poly(our.x, 2) + poly(our.y, 2) + I(our.x * our.y), data=data, family="binomial")
#  fieldingplot(mod, data=data, write.pdf=TRUE)
  return(mod)
}

getModelFieldingRF = function (data) {
  mod = glm((fielderPos == "RF") ~ poly(our.x, 2) + poly(our.y, 2) + I(our.x * our.y), data=data, family="binomial")
  return(mod)
}

#' 
#' @title getModelFieldingCollective
#' 
#' @description Determine the responsibility of the fielders, collectively
#' 
#' @details Computes a 2D kernel smoothed estimate of the probability that *any* of the 9 fielders
#' will make a play on a ball in play
#' 
#' @param data An MLBAM data.frame 
#' 
#' @return a vector representing the probability that each ball in play will be fielded
#' 
#' @export
#' @examples
#' 
#' ds = getData()
#' ds$resp.field = getModelFieldingCollective(ds)
#' 

getModelFieldingCollective = function (data) {
  require(KernSmooth)
  message("....Computing the collective fielding model...")
  outs = subset(data, wasFielded, select=c("our.x", "our.y"))
  hits = subset(data, !wasFielded, select=c("our.x", "our.y"))
  # Find 2D kernel density estimates for hits and outs
  # Make sure to specify the range, so that they over estimated over the same grid
  grid = list(range(data$our.x, na.rm=TRUE), range(data$our.y, na.rm=TRUE))
  fit.out <- bkde2D(outs, bandwidth = c(10,10), range.x = grid)
  fit.hit <- bkde2D(hits, bandwidth = c(10,10), range.x = grid)
  
  field.smooth = data.frame(cbind(expand.grid(fit.out$x1, fit.out$x2), isOut = as.vector(fit.out$fhat)), isHit = as.vector(fit.hit$fhat))
  names(field.smooth)[1:2] = c("x", "y")
  # Plot the surfaces
  #  wireframe(isOut ~ x + y, data=field.smooth, scales = list(arrows = FALSE), drape = TRUE, colorkey = TRUE)
  #  wireframe(isHit ~ x + y, data=field.smooth, scales = list(arrows = FALSE), drape = TRUE, colorkey = TRUE)
  
  # Make sure to add a small amount to avoid division by zero
  field.smooth = transform(field.smooth, wasFielded = isOut / (isOut + isHit + 0.00000001))
  # summary(field.smooth)
#  fieldingplot(wasFielded ~ x + y, data=field.smooth, label = "cum_resp", write.pdf=TRUE)
  
  fit.all = function (x, y) {
    require(Hmisc)
    x.idx = whichClosest(field.smooth$x, x)
    y.idx = whichClosest(field.smooth$y, y)
    match = subset(field.smooth, x == field.smooth$x[x.idx] & y == field.smooth$y[y.idx])
    return(match$wasFielded)
  }
  
  message("....Applying the collective fielding model...")
  resp.field = mapply(fit.all, data$our.x, data$our.y)
  return(resp.field)
}



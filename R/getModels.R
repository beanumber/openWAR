#' @title getModels
#' @aliases getModels.GameDayPlays
#' 
#' @description Retrieve various models trained on GameDayPlays data
#' 
#' @details This function will retrieve various models based on the MLBAM data 
#' set and the openWAR framework. Currently this only returns the Run Expectancy Model.
#' 
#' @param data a GameDayPlays dataset
#' @param ... currently ignored
#' 
#' @return A list of model objects
#' 
#' @export getModels
#' @examples
#' 
#' data(May)
#' re.mod = getModels(May, type = 'run-expectancy')
#' 
#' # Generate the Run Expectancy Matrix
#' states = expand.grid(startCode = 0:7, startOuts = 0:2)
#' matrix(predict(re.mod[["run-expectancy"]], newdata=states), ncol=3)

getModels = function(data, ...) UseMethod("getModels")

#' @export
#' @rdname getModels
#' @method getModels GameDayPlays
#' 
getModels.GameDayPlays = function(data, ...) {
    models = list()
    models[["run-expectancy"]] = getModelRunExpectancy(data)
    
    return(models)
}

#' @title getModelRunExpectancy
#' @aliases getModelRunExpectancy
#' 
#' @description Build the Run Expectancy Model
#' 
#' @details This function will build the Run Expectancy Model used in \code{openWAR}.
#' 
#' @param data a GameDayPlays dataset
#' @param mod.re an existing Run Expectancy Model
#' @param verbose print messages to screen during operation?
#' @param drop.incomplete a LOGICAL indicating whether incomplete innings (e.g. walk-off innings)
#' should be excluded
#' @param ... currently ignored
#' 
#' @return An \code{\link{lm}} object 
#' 
#' @export getModelRunExpectancy
#' @examples
#' 
#' data(May)
#' re.mod <- getModelRunExpectancy(May)
#' 
#' # Display the Run Expectancy Matrix
#' states = expand.grid(startCode = 0:7, startOuts = 0:2)
#' matrix(predict(re.mod, newdata=states), ncol=3)

getModelRunExpectancy = function(data, mod.re = NULL, verbose = TRUE, drop.incomplete = TRUE, ...) UseMethod("getModelRunExpectancy")

#' @export
#' @rdname getModelRunExpectancy
#' @method getModelRunExpectancy default

getModelRunExpectancy.default = function(data, mod.re = NULL, verbose = TRUE, drop.incomplete = TRUE, ...) {
  return(getModelRunExpectancy.GameDayPlays(data, mod.re, verbose, drop.incomplete, ...))
}


#' @export
#' @rdname getModelRunExpectancy
#' @method getModelRunExpectancy GameDayPlays

getModelRunExpectancy.GameDayPlays = function(data, mod.re = NULL, verbose = TRUE, drop.incomplete = TRUE, ...) {
    # Check to see whether the supplied run expectancy model has a predict() method
    if (!paste("predict", class(mod.re), sep = ".") %in% methods(predict)) {
        message("....Supplied Run Expectancy model does not have a predict method...")
        message("....Building in-sample Run Expectancy Model...")
        # Drop incomplete innings
        if (drop.incomplete) {
            ds <- dplyr::filter_(data, ~outsInInning == 3)
        } else {
            ds <- data
        }
        # use model=FALSE option to decrease memory footprint Note that qr=TRUE is necessary to use predict() later
        mod.re = lm(runsFuture ~ as.factor(startCode) * as.factor(startOuts), data = ds, model = FALSE)
    }
    
    if (verbose) {
        message("....Run Expectancy Model....")
        states = expand.grid(startCode = 0:7, startOuts = 0:2)
        print(matrix(predict(mod.re, newdata = states), ncol = 3))
    }
    return(mod.re)
}

getModelPitching = function(data, mod.pitch = NULL, verbose = TRUE) {
    if (!paste("predict", class(mod.pitch), sep = ".") %in% methods(predict)) {
        message("....Supplied Pitching model does not have a predict method...")
        message("....Building in-sample Pitching Model...")
        mod.pitch = lm(delta.pitch ~ factor(venueId) + (throws == stand), data = data[, c("delta.pitch", "venueId", "throws", 
            "stand")])
    }
    return(mod.pitch)
}

getModelOffense = function(data, mod.off = NULL, verbose = TRUE) {
    # Control for circumstances
    if (!paste("predict", class(mod.off), sep = ".") %in% methods(predict)) {
        message("....Supplied Offense model does not have a predict method...")
        message("....Building in-sample Offense Model...")
        mod.off = lm(delta ~ factor(venueId) + (throws == stand), data = data[, c("delta", "venueId", "throws", "stand")])
    }
    return(mod.off)
}

getModelBaserunning = function(data, mod.br = NULL, verbose = TRUE) {
    # Siphon off the portion attributable to the baserunners
    if (!paste("predict", class(mod.br), sep = ".") %in% methods(predict)) {
        message("....Supplied Baserunning model does not have a predict method...")
        message("....Building in-sample Baserunning Model...")
        mod.br = lm(delta.off ~ event * as.factor(startCode) * as.factor(startOuts), data = data[, c("delta.off", "event", "startCode", 
            "startOuts")])
    }
    return(mod.br)
}

getModelBatting = function(data, mod.bat = NULL, verbose = TRUE) {
    if (!paste("predict", class(mod.bat), sep = ".") %in% methods(predict)) {
        message("....Supplied Batting model does not have a predict method...")
        message("....Building in-sample Batting Model...")
        mod.bat = lm(delta.bat ~ as.factor(batterPos), data = data[, c("delta.bat", "batterPos")])
    }
    return(mod.bat)
}

getModelFieldingPosition = function(data, position) {
    mod = glm((fielderPos == position) ~ poly(our.x, 2) + poly(our.y, 2) + I(our.x * our.y), data = data, family = "binomial")
    return(mod)
}

getModelFieldingPitcher = function(data) {
    mod = glm((fielderPos == "P") ~ poly(our.x, 2) + poly(our.y, 2) + I(our.x * our.y), data = data, family = "binomial")
    return(mod)
}

getModelFieldingCatcher = function(data) {
    mod = glm((fielderPos == "C") ~ poly(our.x, 2) + poly(our.y, 2) + I(our.x * our.y), data = data, family = "binomial")
    return(mod)
}

getModelFielding1B = function(data) {
    mod = glm((fielderPos == "1B") ~ poly(our.x, 2) + poly(our.y, 2), data = data, family = "binomial")
    return(mod)
}

getModelFielding2B = function(data) {
    mod = glm((fielderPos == "2B") ~ poly(our.x, 2) + poly(our.y, 2) + I(our.x * our.y), data = data, family = "binomial")
    return(mod)
}

getModelFielding3B = function(data) {
    mod = glm((fielderPos == "3B") ~ poly(our.x, 2) + poly(our.y, 2) + I(our.x * our.y), data = data, family = "binomial")
    return(mod)
}

getModelFieldingSS = function(data) {
    mod = glm((fielderPos == "SS") ~ poly(our.x, 2) + poly(our.y, 2) + I(our.x * our.y), data = data, family = "binomial")
    return(mod)
}

getModelFieldingLF = function(data) {
    mod = glm((fielderPos == "LF") ~ poly(our.x, 2) + poly(our.y, 2) + I(our.x * our.y), data = data, family = "binomial")
    return(mod)
}

getModelFieldingCF = function(data) {
    mod = glm((fielderPos == "CF") ~ poly(our.x, 2) + poly(our.y, 2) + I(our.x * our.y), data = data, family = "binomial")
    # fieldingplot(mod, data=data, write.pdf=TRUE)
    return(mod)
}

getModelFieldingRF = function(data) {
    mod = glm((fielderPos == "RF") ~ poly(our.x, 2) + poly(our.y, 2) + I(our.x * our.y), data = data, family = "binomial")
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
#' 
#' @importFrom KernSmooth bkde2D
#' @importFrom Hmisc whichClosest
#' 
#' 

getModelFieldingCollective = function(data) {
    message("....Computing the collective fielding model...")
    data = dplyr::mutate_(data, wasFielded = ~!is.na(fielderId))
    outs = dplyr::select_(dplyr::filter_(data, ~wasFielded == TRUE), ~our.x, ~our.y)
    hits = dplyr::select_(dplyr::filter_(data, ~wasFielded == FALSE), ~our.x, ~our.y)
    # Find 2D kernel density estimates for hits and outs Make sure to specify the range, so that they over estimated over the
    # same grid
    grid = list(range(data$our.x, na.rm = TRUE), range(data$our.y, na.rm = TRUE))
    fit.out <- KernSmooth::bkde2D(outs, bandwidth = c(10, 10), range.x = grid)
    fit.hit <- KernSmooth::bkde2D(hits, bandwidth = c(10, 10), range.x = grid)
    
    field.smooth = data.frame(cbind(expand.grid(fit.out$x1, fit.out$x2), isOut = as.vector(fit.out$fhat)), isHit = as.vector(fit.hit$fhat))
    names(field.smooth)[1:2] = c("x", "y")
    # Plot the surfaces wireframe(isOut ~ x + y, data=field.smooth, scales = list(arrows = FALSE), drape = TRUE, colorkey =
    # TRUE) wireframe(isHit ~ x + y, data=field.smooth, scales = list(arrows = FALSE), drape = TRUE, colorkey = TRUE)
    
    # Make sure to add a small amount to avoid division by zero
    field.smooth = dplyr::mutate_(field.smooth, wasFielded = ~(isOut/(isOut + isHit + 1e-08)))
    # summary(field.smooth) fieldingplot(wasFielded ~ x + y, data=field.smooth, label = 'cum_resp', write.pdf=TRUE)
    
    fit.all = function(x, y) {
        x.idx = Hmisc::whichClosest(field.smooth$x, x)
        y.idx = Hmisc::whichClosest(field.smooth$y, y)
        match = dplyr::filter_(field.smooth, ~x == field.smooth$x[x.idx] & y == field.smooth$y[y.idx])
        return(match$wasFielded)
    }
    
    message("....Applying the collective fielding model...")
    resp.field = mapply(fit.all, data$our.x, data$our.y)
    return(resp.field)
} 

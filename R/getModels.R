#' @title Retrieves prior estimated models.
#' 
#' @description Retrieve various models trained on \code{\link{GameDayPlays}} data.
#' 
#' @details This function will retrieve various models based on the MLBAM data 
#' set and the openWAR framework. Currently this only returns the Run Expectancy Model.
#' 
#' @param data a \code{\link{GameDayPlays}} dataset
#' @param ... currently ignored
#' 
#' @return A \code{\link{list}} of model objects
#' 
#' @export
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
#' 
#' @description Build the Run Expectancy Model
#' 
#' @details This function will build the Run Expectancy Model used in \code{\link{openWAR}}.
#' 
#' @param data a \code{\link{GameDayPlays}} dataset
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
  return(getModelRunExpectancy(data, mod.re, verbose, drop.incomplete, ...))
}


#' @export
#' @importFrom utils methods
#' @rdname getModelRunExpectancy
#' @method getModelRunExpectancy GameDayPlays

getModelRunExpectancy.GameDayPlays = function(data, mod.re = NULL, verbose = TRUE, drop.incomplete = TRUE, ...) {
    # Check to see whether the supplied run expectancy model has a predict() method
    if (!paste("predict", class(mod.re), sep = ".") %in% utils::methods(predict)) {
        message("....Supplied Run Expectancy model does not have a predict method...")
        message("....Building in-sample Run Expectancy Model...")
        # Drop incomplete innings
        if (drop.incomplete) {
            ds <- dplyr::filter_(data, ~outsInInning == 3)
        } else {
            ds <- data
        }
        # use model=FALSE option to decrease memory footprint Note that qr=TRUE is necessary to use predict() later
        mod.re <- lm(runsFuture ~ as.factor(startCode) * as.factor(startOuts), data = ds, model = FALSE)
    }
    
    if (verbose) {
        print(getMatrixRunExpectancy(mod.re))
    }
    return(mod.re)
}

#' getMatrixRunExpectancy
#' @description a untility function for retrieving a run expectancy
#' model in matrix form.
#' @export
#' @param x a run expectancy model
#' @param ... currently ignored
#' @return a matrix in the familiar 8x3 form
#' @examples 
#' 
#' ERM <- getMatrixRunExpectancy(getModelRunExpectancy(May, verbose = FALSE))
#' dim(ERM)
#' ERM

getMatrixRunExpectancy <- function(x, ...) {
  message("....Run Expectancy Model....")
  states = expand.grid(startCode = 0:7, startOuts = 0:2)
  return(matrix(predict(x, newdata = states), ncol = 3))
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

#' @importFrom stats glm

getModelFieldingPosition = function(data, position) {
    mod = stats::glm((fielderPos == position) ~ poly(our.x, 2) + poly(our.y, 2) + I(our.x * our.y), data = data, family = "binomial")
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
#' @param data A \code{\link{GameDayPlays}} object
#' 
#' @return a vector representing the probability that each ball in play will be fielded
#' 
#' @export
#' 
#' @importFrom KernSmooth bkde2D
#' @importFrom Hmisc whichClosest
#' 
#' @examples 
#' 
#' fmod <- getModelFieldingCollective(May)
#' plotFielding(fmod)
#' 

getModelFieldingCollective = function(data) { UseMethod("getModelFieldingCollective"); }

#' @export
#' @rdname getModelFieldingCollective
#' @method getModelFieldingCollective GameDayPlays

getModelFieldingCollective.GameDayPlays = function(data) {
  getModelFieldingCollective(dplyr::filter_(data, ~isBIP == TRUE))
}

#' @export
#' @importFrom stats na.omit
#' @rdname getModelFieldingCollective
#' @method getModelFieldingCollective default
#' 
getModelFieldingCollective.default = function(data) {
    message("....Computing the collective fielding model...")
    data <- dplyr::mutate_(data, wasFielded = ~!is.na(fielderId))
    outs <- data %>%
      dplyr::filter_(~wasFielded == TRUE) %>%
#      dplyr::filter_(~!is.na(our.x)) %>%
      dplyr::select_(~our.x, ~our.y)
    hits <- data %>%
      dplyr::filter_(~wasFielded == FALSE) %>%
#      dplyr::filter_(~!is.na(our.x)) %>%
      dplyr::select_(~our.x, ~our.y)
    # Find 2D kernel density estimates for hits and outs Make sure to specify the range, so that they over estimated over the
    # same grid
    grid = list(range(data$our.x, na.rm = TRUE), range(data$our.y, na.rm = TRUE))
    fit.out <- KernSmooth::bkde2D(as.matrix(stats::na.omit(outs)), bandwidth = c(10, 10), range.x = grid)
    fit.hit <- KernSmooth::bkde2D(as.matrix(stats::na.omit(hits)), bandwidth = c(10, 10), range.x = grid)
    class(fit.out) <- union("bkde2D", class(fit.out))
    class(fit.hit) <- union("bkde2D", class(fit.hit))
    
    fmod <- fit.out
    fmod$fhat <- fit.out$fhat / (fit.out$fhat + fit.hit$fhat + 1e-08)
    return(fmod)
}

#' @export

predict.bkde2D <- function(object, ...) {
  dots <- list(...)
  newdata <- dots$newdata
  if (ncol(newdata) < 2) {
    stop("newdata must have at least two columns")
  }
  # find the indices that match closest
  x.idx = Hmisc::whichClosest(object$x1, as.numeric(as.data.frame(newdata)[,1]))
  y.idx = Hmisc::whichClosest(object$x2, as.numeric(as.data.frame(newdata)[,2]))
  zHat <- apply(data.frame(x.idx, y.idx), MARGIN = 1, function(x) { object$fhat[x[1], x[2]]; })
  return(zHat)
}


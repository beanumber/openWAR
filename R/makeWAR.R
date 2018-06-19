#' @title Compute the runs above average (RAA)  for each play.
#' 
#' @description Computes runs above average (RAA) for each player involved in each play of the \code{\link{GameDayPlays}} object. 
#' 
#' @details Within a \code{\link{GameDayPlays}} object, each row consists of a single plate appearance and contains information about the batter, all of the baserunners,
#'  the pitcher, and all of the fielders on the field during the plate appearance. The total value of the play as determined by the change in the run expectancy 
#'  matrix from the beginning of the plate appearence to the end of the plate appearance is partitioned across all players involved in the play on offense, 
#'  and that same value (with the opposite sign) is partitioned across the pitcher and all of the fielders.  Thus for every single plate appearance a runs above average (RAA) value is assigned
#'  to every player involved in the play.  
#'  If no \code{models} argument is supplied, then all
#' models necessary for the computation of openWAR will be generated on the data set given. 
#' The output of this function is then used in the function \code{\link{getWAR}} to calculate a Wins Above Replacement (WAR) value for each player. 
#' 
#' If \code{verbose == TRUE}, then various pieces of information will be displayed during the computation.
#' 
#' The \code{\link{makeWAR}} algorithm consists of seven steps, each of which appends columns to the original data frame.
#' \itemize{
#'  \item Compute the change in run expectancy (\eqn{\delta}) on each plate appearance
#'  \item Partition \eqn{-\delta} among the fielders using \code{\link{makeWARFielding}}
#'  \item Partition the remaining \eqn{\delta} for the pitcher using \code{getModelPitching}
#'  \item Adjust \eqn{\delta} for cicumstance, and attribute it to the offense using \code{getModelOffense}
#'  \item Partition some of \eqn{\delta} for the baserunners, using \code{getModelBaserunning}
#'  \item Attribute the rest of \eqn{\delta} to the batter, and adjust using \code{getModelBatting}
#'  \item Parition the baserunning \eqn{\delta} to the various baserunners, using \code{\link{makeWARBaserunning}}
#' }
#' 
#' Elements of \code{models}:
#' \itemize{
#' \item{run-expectancy}{: a model for assigning a run expectancy value to any of the 24 (base,out) states. Variables
#' must be \code{startCode} [0-7] and \code{startOuts} [0-2]}
#' \item{pitching}{: a model for the expected outcome of a plate appearance attributable to the pitcher. Variables
#' must be \code{venueId}, \code{throws} [L/R], and \code{stands} [L/R]}
#' \item{offense}{: a model for the expected outcome of a plate appearance attributable to the offense. Variables
#' must be \code{venueId}, \code{throws} [L/R], and \code{stands} [L/R]}
#' \item{baserunning}{: a model for the expected contribution of the baserunners to a plate appearance. Variables
#' must be \code{event} (the type of batting event), \code{startCode} [0-7], and \code{startOuts} [0-2]}
#' \item{batting}{: a model for the expected contribution of the batter to a plate appearance. Variables
#' must be \code{batterPos} (the defensive position of the batter)}
#' }
#' 
#' @param x An object of class \code{\link{GameDayPlays}}_*, where * can be the output of 
#' and of the seven steps listed below.
#' @param models A named list of models, each with a \code{\link{predict}} method. See Details.
#' @param verbose A \code{logical} indicating whether you want various messages 
#' and information to be displayed during the computation
#' @param low.memory A \code{logical} indicating whether to conserve memory by 
#' not storing the model objects. 
#' @param step A \code{logical} indicating whether the function should stop after one of the seven steps.
#' @param ... currently ignored
#' 
#' @return An object of class \code{\link{openWARPlays}} which is a list containing the following: 
#' \itemize{
#'  \item{data}{A data.frame containing the original data along with appended 
#'  rows containing the RAA values for each player involved in a plate appearance.
#'  NULL if \code{low.memory} is TRUE.}
#'  \item{models.used}{A list containing all of the model information for each 
#'  of the models used in computing RAA. NULL if \code{low.memory} is TRUE.}
#'  \item{openWARPlays}{A data.frame of class \code{\link{openWARPlays}} containing only the columns necessry for input into the getWAR function.  }
#' }
#' 
#' @import dplyr
#' @importFrom stringr str_count
#' @export makeWAR
#' @examples
#' 
#' \dontrun{
#' res <- makeWAR(May)
#' summary(getWAR(res))
#' 
#' print(object.size(res), units = "Mb")
#' res <- makeWAR(May, low.memory = FALSE)
#' print(object.size(res), units = "Mb"
#' names(res$models.used)
#' }

makeWAR = function(x, models = list(), verbose = TRUE, low.memory = TRUE, step = FALSE, ...) UseMethod("makeWAR")

#' @export
#' @rdname makeWAR
#' @method makeWAR GameDayPlays
#' @examples 
#' res <- makeWAR(May, step = TRUE)
#' print(object.size(res), units = "Mb")

makeWAR.GameDayPlays = function(x, models = list(), verbose = TRUE, low.memory = TRUE, step = FALSE, ...) {
  class(x) <- unique(c("GameDayPlays", class(dplyr::tbl_df(x))))
  x$idx <- 1:nrow(x)
  # Step 1: Define \delta, the change in expected runs
  mod.re = getModelRunExpectancy(select_(x, "outsInInning", "runsFuture", "startCode", "startOuts"), 
                                 models[["run-expectancy"]], verbose)
  models.used <- list(`run-expectancy` = mod.re)
  deltas = makeWARre24(select_(x, "startCode", "startOuts", "endCode", "endOuts", "runsOnPlay"), 
                       mod.re, verbose)
  data <- bind_cols(x, deltas)
  
  # Add the new class
  out <- list(data = data, models.used = models.used)
  class(out) = c("GameDayPlays_Runs", class(out))
  if (step) {
    return(out)
  } else {
    makeWAR(out, models, verbose, low.memory, step, ...)
  }
}


#' @export
#' @rdname makeWAR
#' @method makeWAR GameDayPlays_Runs

    
makeWAR.GameDayPlays_Runs = function(x, models = list(), verbose = TRUE, low.memory = TRUE, step = FALSE, ...) {
    ############### Step 2: Define RAA for the defense Work only with the subset
    ############### of data for which the ball is in play and keep track of the 
    ############### indices 
  bip.idx <- which(x$data$isBIP == TRUE & !is.na(x$data$our.x))
  ds.field <- x$data[bip.idx, ]
  fielding <- makeWARFielding(ds.field, models, verbose)
  
  for (col.name in names(fielding)) {
      x$data[bip.idx, col.name] <- fielding[, col.name]
  }
  # Add the new class
  class(x) = c("GameDayPlays_Fielding", class(x))
  if (step) {
    return(x)
  } else {
    makeWAR(x, models, verbose, low.memory, step, ...)
  }
}



#' @export
#' @rdname makeWAR
#' @method makeWAR GameDayPlays_Fielding


makeWAR.GameDayPlays_Fielding = function(x, models = list(), verbose = TRUE, low.memory = TRUE, step = FALSE, ...) {
  # Step 3: Define RAA for the pitcher
  x$data <- mutate_(x$data, delta.pitch = ~ifelse(is.na(delta.field), delta, delta - delta.field))
  message("...Estimating Pitching Runs Above Average...")
  mod.pitch <- getModelPitching(select_(x$data, "delta.pitch", "venueId", "throws", "stand"), 
                               models[["pitching"]], verbose)
  if (verbose) {
    message("....Pitching Model....")
    print(sort(coef(mod.pitch)))
  }
  
  # append the model used to the list of models
  x$models.used[["pitching"]] <- mod.pitch
  # Note the pitcher RAA's are the negative residuals!
  # mutate_ is thrownig an error here.
  #x$data <- mutate_(x$data, raa.pitch = ~predict(mod.pitch, newdata = x$data[, c("venueId", "throws", "stand")]) - delta.pitch)
  x$data$raa.pitch <- predict(mod.pitch, newdata = x$data[, c("venueId", "throws", "stand")]) - x$data$delta.pitch 
  
  # Add the new class
  class(x) = c("GameDayPlays_Pitching", class(x))
  if (step) {
    return(x)
  } else {
    makeWAR(x, models, verbose, low.memory, step, ...)
  }
}

#' @export
#' @rdname makeWAR
#' @method makeWAR GameDayPlays_Pitching
    
makeWAR.GameDayPlays_Pitching = function(x, models = list(), verbose = TRUE, low.memory = TRUE, step = FALSE, ...) {    
  # Step 4: Define RAA for the batter
  message("...Building model for offense...")
  mod.off <- getModelOffense(select_(x$data, "delta", "venueId", "throws", "stand"), 
                            models[["offense"]], verbose)
  if (verbose) {
    message("....Offense Model....")
    print(sort(coef(mod.off)))
  }
  x$models.used[["offense"]] <- mod.off
  # delta.off is the contribution above average of the batter AND all of the runners
  # mutate throws an error here.
  #x$data <- mutate_(x$data, delta.off = ~delta - predict(mod.off, newdata = x$data[, c("venueId", "throws", "stand")]))
  x$data$delta.off <- x$data$delta - predict(mod.off, newdata = x$data[, c("venueId", "throws", "stand")])
  
  # Add the new class
  class(x) = c("GameDayPlays_Offense", class(x))
  if (step) {
    return(x)
  } else {
    makeWAR(x, models, verbose, low.memory, step, ...)
  }
}    

#' @export
#' @importFrom stats coef
#' @rdname makeWAR
#' @method makeWAR GameDayPlays_Offense
    
makeWAR.GameDayPlays_Offense = function(x, models = list(), verbose = TRUE, low.memory = TRUE, step = FALSE, ...) {    
  # If runners are on base, partition delta between the batter and baserunners
  br.idx <- which(x$data$startCode > 0)
  message("...Partitioning Offense into Batting and Baserunning...")
  mod.br <- getModelBaserunning(x$data[br.idx, c("delta.off", "event", "startCode", "startOuts")], 
                               models[["baserunning"]], verbose)
  if (verbose) {
    message("....Baserunning Model....")
    message(paste("....", length(stats::coef(mod.br)), "coefficients -- suppressing output..."))
    # print(sort(coef(mod.br)))
  }
  x$models.used[["baserunning"]] <- mod.br
  # delta.br is the contribution of the baserunners beyond the event type
  x$data[br.idx, "delta.br"] <- x$data[br.idx, "delta.off"] - predict(mod.br, newdata = x$data[br.idx, c("event", "startCode", "startOuts")])
  # Add the new class
  class(x) = c("GameDayPlays_Baserunning", class(x))
  if (step) {
    return(x)
  } else {
    makeWAR(x, models, verbose, low.memory, step, ...)
  }
}    

#' @export
#' @rdname makeWAR
#' @method makeWAR GameDayPlays_Baserunning

makeWAR.GameDayPlays_Baserunning = function(x, models = list(), verbose = TRUE, low.memory = TRUE, step = FALSE, ...) {
  # Whatever is left over goes to the batter -- just control for defensive position
  x$data <- mutate_(x$data, delta.bat = ~ifelse(is.na(delta.br), delta, delta - delta.br))
  message("...Estimating Batting Runs Above Average...")
  mod.bat <- getModelBatting(select_(x$data, "delta.bat", "batterPos"), 
                            models[["batting"]], verbose)
  if (verbose) {
    message("....Batting Model....")
    print(sort(coef(mod.bat)))
  }
  x$models.used[["batting"]] <- mod.bat
  # Control for batter position Note that including 'idx' is not necessary -- it just ensures that the argument passed is a
  # data.frame
  # mutate throws error here.
  #x$data <- mutate_(x$data, raa.bat = ~delta.bat - predict(mod.bat, newdata = x$data[, c("batterPos", "idx")]))
  x$data$raa.bat <- x$data$delta.bat - predict(mod.bat, newdata = x$data[, c("batterPos", "idx")])
  # Add the new class
  class(x) = c("GameDayPlays_Batters", class(x))
  if (step) {
    return(x)
  } else {
    makeWAR(x, models, verbose, low.memory, step, ...)
  }
}

#' @export
#' @rdname makeWAR
#' @method makeWAR GameDayPlays_Batters
#' @examples 
#' \dontrun{
#' res <- makeWAR(May, step = TRUE)
#' print(object.size(res), units = "Mb")
#' 
#' res2 <- makeWAR(res, step = TRUE)
#' print(object.size(res2), units = "Mb")
#' 
#' res3 <- makeWAR(res2, step = TRUE)
#' print(object.size(res3), units = "Mb")
#' 
#' res4 <- makeWAR(res3, step = TRUE)
#' print(object.size(res4), units = "Mb")
#' 
#' res5 <- makeWAR(res4, step = TRUE)
#' print(object.size(res5), units = "Mb")
#' 
#' res6 <- makeWAR(res5, step = TRUE)
#' print(object.size(res6), units = "Mb")
#' 
#' res7 <- makeWAR(res6, step = TRUE)
#' print(object.size(res7), units = "Mb")
#' }
  
makeWAR.GameDayPlays_Batters = function(x, models = list(), verbose = TRUE, low.memory = TRUE, step = FALSE, ...) {
  ############### Step 5: Define RAA for the baserunners
  br.idx <- which(x$data$startCode > 0)
  br.fields <- c("idx", "delta.br", "start1B", "start2B", "start3B", "end1B", "end2B", "end3B", "runnerMovement", "event", 
                "startCode", "startOuts")
  raa.br <- makeWARBaserunning(x$data[br.idx, br.fields], models[["baserunning"]], verbose)
  x$data <- merge(x = x$data, y = raa.br, by = "idx", all.x = TRUE)

  # include the computations as a separate data.frame
  id.fields <- c("batterId", "start1B", "start2B", "start3B", "pitcherId", "playerId.C", "playerId.1B", "playerId.2B", "playerId.3B", 
                "playerId.SS", "playerId.LF", "playerId.CF", "playerId.RF", "batterName", "pitcherName", "gameId", "event", "isPA")
  delta.fields <- c("delta", "delta.field", "delta.pitch", "delta.br", "delta.bat")
  raa.fields <- c("raa.bat", "raa.br1", "raa.br2", "raa.br3", "raa.pitch", "raa.P", "raa.C", "raa.1B", "raa.2B", "raa.3B", 
                 "raa.SS", "raa.LF", "raa.CF", "raa.RF")
  x$openWARPlays <- x$data[, c(id.fields, delta.fields, raa.fields)]
  class(x$openWARPlays) = c("openWARPlays", class(x$openWARPlays))  
  # Add the new class
  class(x) <- "list"
  if (low.memory) {
    x$models.used <- NULL
    x$data <- NULL
  }
  return(x)
}


#' @title Compute RE24 before and after each PA.
#' 
#' @description Compute the Run Expectancy at the beginning and end of every play
#' in a data frame, as well as their difference (RE24).
#' 
#' @param data A \code{GameDayPlays} object
#' @param mod.re an \code{lm} object giving the Run Expectancy Model. If 
#' NULL (the default), the run expectancy model will be generated from \code{data}
#' @param ... currently ignored
#' 
#' @return a data frame with three columns: \code{startExR}, \code{endExR}, and 
#' \code{delta}. The latter is the change in run expectancy.
#' 
#' @importFrom stats predict
#' @export
#' @examples
#' 
#' Maymod.re = getModelRunExpectancy(May)
#' re24 <- makeWARre24(May14)
#' re24full <- makeWARre24(May14, mod.re = Maymod.re)
#' sum(re24$delta)
#' sum(re24full$delta)


makeWARre24 = function(data, mod.re = NULL, ...) {
    message("...Estimating Expected Runs...")
    
    if (is.null(mod.re)) {
      mod.re = getModelRunExpectancy(data)
    }
    
    begin.states = data[, c("startCode", "startOuts")]
    end.states = data[, c("endCode", "endOuts")]
    end.states$endOuts = with(end.states, ifelse(endOuts == 3, NA, endOuts))
    names(end.states) = names(begin.states)
    
    startExR = predict(mod.re, newdata = begin.states)
    endExR = predict(mod.re, newdata = end.states)
    endExR = ifelse(is.na(endExR), 0, endExR)
    
    out = data.frame(startExR, endExR)
    out$delta = endExR - startExR + data$runsOnPlay
    return(out)
}


#' @title Compute RAA for fielders.
#' 
#' @description Compute the RAA values for all of the fielders.  Used internally in \code{makeWAR}.
#' 
#' @param data A \code{GameDayPlays} object
#' @param ... currently ignored
#' 
#' @return A data.frame with 12 columns: \code{p.hat} contains the probability that any fielder makes an out given the location 
#' of the ball in play; \code{delta.field} is the total delta assigned to all of the fielders; columns 3 through 11 
#' contains the delta values for each od the 9 fielders (i.e. these columns sum to \code{delta.field}); \code{venueId} contains information about
#' the stadium that the plate appearance took place in.  
#' 
#' 
#' 


makeWARFielding = function(data, ...) {
    message("...Estimating Fielding Runs Above Average...")
    
    # Compute the collective responsibility of all fielders
    fmod <- getModelFieldingCollective(data[, c("fielderId", "our.x", "our.y")])
    message("....Applying the collective fielding model...")
    p.hat <- predict(fmod, newdata = select_(data, ~our.x, ~our.y))
    # Step 2a: Define \delta.field for the defense, collectively
    delta.field <- with(data, ifelse(endOuts == startOuts, 
                                  delta * p.hat, delta * (1 - p.hat)))
    # Compute the individual responsibility of each fielder
    P <- getFielderResp(data)
    # Step 2b: Define \delta.field for the defense, individually
    delta.fielders <- delta.field * P
    names(delta.fielders) <- gsub("resp", "delta", names(delta.fielders))
    
    out <- data.frame(p.hat, delta.field, delta.fielders)
    
    # Normalize the delta's into RAA's
    raa.field <- getFielderRAA(cbind(out, venueId = data$venueId))
    return(cbind(out, raa.field))
}


#' 
#' @title Tabulate RAA for fielders.
#' 
#' @description Determine the Runs Above Average (RAA) of the fielders.
#' 
#' @details RAA is the residuals from a simple fielding model.  Used in the function \code{makeWARFielding}
#' 
#' @param data A data.frame containg an estimate of the probably that a ball on a given play would be fielded, the total delta for fielders, a column of delta for each fielder, and a venueId
#' 
#' @return A data.frame containing fielding RAA values for all plate appearances with a ball in play
#' 
#' @export
#' @importFrom stats lm
#' 

getFielderRAA = function(data) {
    # Build a model for each fielder's expected change in runs
    mod.P = stats::lm(delta.P ~ factor(venueId), data = data)
    mod.C = stats::lm(delta.C ~ factor(venueId), data = data)
    mod.1B = stats::lm(delta.1B ~ factor(venueId), data = data)
    mod.2B = stats::lm(delta.2B ~ factor(venueId), data = data)
    mod.3B = stats::lm(delta.3B ~ factor(venueId), data = data)
    mod.SS = stats::lm(delta.SS ~ factor(venueId), data = data)
    mod.LF = stats::lm(delta.LF ~ factor(venueId), data = data)
    mod.CF = stats::lm(delta.CF ~ factor(venueId), data = data)
    mod.RF = stats::lm(delta.RF ~ factor(venueId), data = data)
    
    # Define RAA to be the residuals from the individual fielders models
    raa = -data.frame(mod.P$residuals, mod.C$residuals, mod.1B$residuals, mod.2B$residuals, mod.3B$residuals, mod.SS$residuals, 
        mod.LF$residuals, mod.CF$residuals, mod.RF$residuals)
    names(raa) = gsub("mod", "raa", gsub(".residuals", "", names(raa)))
    
    # The column-wise sums should all be zero colSums(raa)
    return(raa)
}




#' @title Compute total fielder responsibility.
#' 
#' @description Find the shared responsibility models for balls in play.
#' 
#' @details Fits 9 logistic regression models, each giving the probability of 
#' a fielder at one of the 9 defensive positions successfully converting the 
#' ball into at least one out.
#' 
#' @param data An MLBAM data.frame 
#' @param ... currently ignored
#' 
#' @return data.frame with 9 columns each representing the probability that a particular fielder will make at least one out; each row represents a ball in play
#' 
#' @export
#' 


getFielderResp = function(data, ...) {
    ds = data
    ds$fielderPos = with(ds, ifelse(is.na(fielderId), "Hit", "Out"))
    ds$fielderPos = with(ds, ifelse(!is.na(fielderId) & fielderId == pitcherId, "P", fielderPos))
    ds$fielderPos = with(ds, ifelse(!is.na(fielderId) & fielderId == playerId.C, "C", fielderPos))
    ds$fielderPos = with(ds, ifelse(!is.na(fielderId) & fielderId == playerId.1B, "1B", fielderPos))
    ds$fielderPos = with(ds, ifelse(!is.na(fielderId) & fielderId == playerId.2B, "2B", fielderPos))
    ds$fielderPos = with(ds, ifelse(!is.na(fielderId) & fielderId == playerId.3B, "3B", fielderPos))
    ds$fielderPos = with(ds, ifelse(!is.na(fielderId) & fielderId == playerId.SS, "SS", fielderPos))
    ds$fielderPos = with(ds, ifelse(!is.na(fielderId) & fielderId == playerId.LF, "LF", fielderPos))
    ds$fielderPos = with(ds, ifelse(!is.na(fielderId) & fielderId == playerId.CF, "CF", fielderPos))
    ds$fielderPos = with(ds, ifelse(!is.na(fielderId) & fielderId == playerId.RF, "RF", fielderPos))
    
    message("....Building a fielding model for each position...")
    mod.P = getModelFieldingPitcher(ds[, c("fielderPos", "our.x", "our.y")])
    mod.C = getModelFieldingCatcher(ds[, c("fielderPos", "our.x", "our.y")])
    mod.1B = getModelFielding1B(ds[, c("fielderPos", "our.x", "our.y")])
    mod.2B = getModelFielding2B(ds[, c("fielderPos", "our.x", "our.y")])
    mod.3B = getModelFielding3B(ds[, c("fielderPos", "our.x", "our.y")])
    mod.SS = getModelFieldingSS(ds[, c("fielderPos", "our.x", "our.y")])
    mod.LF = getModelFieldingLF(ds[, c("fielderPos", "our.x", "our.y")])
    mod.CF = getModelFieldingCF(ds[, c("fielderPos", "our.x", "our.y")])
    mod.RF = getModelFieldingRF(ds[, c("fielderPos", "our.x", "our.y")])
    
    # mod = mod.CF summary(mod) fit = makeFun(mod) plotFun(fit(x,y) ~ x + y, surface=TRUE, alpha=0.9 , xlim = c(-350, 350), ylim
    # = c(0, 550) , xlab = 'Horizontal Distance from Home Plate (ft.)' , ylab = 'Vertical Distance from Home Plate (ft.)' , zlab
    # = 'Probability of Making a Play' )
    
    out = data.frame(mod.P$fitted, mod.C$fitted, mod.1B$fitted, mod.2B$fitted, mod.3B$fitted, mod.SS$fitted, mod.LF$fitted, 
        mod.CF$fitted, mod.RF$fitted)
    row.sums = apply(out, 1, sum)
    out = out/row.sums
    names(out) = c("resp.P", "resp.C", "resp.1B", "resp.2B", "resp.3B", "resp.SS", "resp.LF", "resp.CF", "resp.RF")
    return(out)
}


#' @title Computes RAA for each baserunner.
#' 
#' @description Calculates the runs above average (RAA) created by each baserunner.  
#' 
#' @details These RAA values are calculated by first computing the expected number of bases 
#' advanced given a starting position (e.g. runner on second) and an event (e.g. Single).  
#' Runners are attributed RAA based on how much they exceeded or underperformed 
#' relative to the expected number of bases advanced given an event.  For example, a baserunner who 
#' advances exactly the expected number of bases given an event would not be attributed any 
#' RAA for that play.  
#' 
#' @param data An MLBAM data.frame 
#' @param ... currently ignored
#' 
#' @return A data.frame with 4 columns: baserunner id, raa for the runner on first base, raa for the runner on second base, and raa for the runner on third base.
#' 
#' @export
#' 


makeWARBaserunning = function(data, ...) {
    message("...Estimating Baserunning Runs Above Average...")
    
    # Figure out what happened to the runner on 3B
    data <- mutate_(data, dest.br3 = ~ifelse(str_count(runnerMovement, paste(start3B, ":3B::T:", sep = "")), "H", NA))
    data <- mutate_(data, dest.br3 = ~ifelse(!is.na(start3B) & !is.na(end3B) & start3B == end3B, "3B", dest.br3))
    
    br3.idx = which(!is.na(data$start3B))
    ds3 = data[br3.idx, ]
    br3.scored = with(ds3, str_count(runnerMovement, paste(start3B, ":3B::T:", sep = "")))
    br3.out = with(ds3, str_count(runnerMovement, paste(start3B, ":3B:::", sep = "")))
    ds3 <- mutate_(ds3, basesAdvanced = ~ifelse(br3.scored == 1, 1, ifelse(br3.out == 1, -3, 0)))
    
    # Figure out what happened to the runner on 2B
    data <- mutate_(data, dest.br2 = ~ifelse(str_count(runnerMovement, paste(start2B, ":2B::T:", sep = "")), "H", NA))
    data <- mutate_(data, dest.br2 = ~ifelse(!is.na(start2B) & !is.na(end3B) & start2B == end3B, "3B", dest.br2))
    data <- mutate_(data, dest.br2 = ~ifelse(!is.na(start2B) & !is.na(end2B) & start2B == end2B, "2B", dest.br2))
    
    br2.idx = which(!is.na(data$start2B))
    ds2 = data[br2.idx, ]
    br2.scored = with(ds2, str_count(runnerMovement, paste(start2B, ":2B::T:", sep = "")))
    br2.out = with(ds2, str_count(runnerMovement, paste(start2B, ":2B:::", sep = "")))
    br2.advanced = with(ds2, str_count(runnerMovement, paste(start2B, ":2B:3B::", sep = "")))
    ds2 <- mutate_(ds2, basesAdvanced = ~ifelse(br2.scored == 1, 2, ifelse(br2.out == 1, -2, ifelse(br2.advanced == 1, 1, 0))))
    
    # Figure out what happened to the runner on 1B
    data <- mutate_(data, dest.br1 = ~ifelse(str_count(runnerMovement, paste(start1B, ":1B::T:", sep = "")), "H", NA))
    data <- mutate_(data, dest.br1 = ~ifelse(!is.na(start1B) & !is.na(end3B) & start1B == end3B, "3B", dest.br1))
    data <- mutate_(data, dest.br1 = ~ifelse(!is.na(start1B) & !is.na(end2B) & start1B == end2B, "2B", dest.br1))
    data <- mutate_(data, dest.br1 = ~ifelse(!is.na(start1B) & !is.na(end1B) & start1B == end1B, "1B", dest.br1))
    
    br1.idx = which(!is.na(data$start1B))
    ds1 = data[br1.idx, ]
    br1.scored = with(ds1, str_count(runnerMovement, paste(start1B, ":1B::T:", sep = "")))
    br1.out = with(ds1, str_count(runnerMovement, paste(start1B, ":1B:::", sep = "")))
    br1.advanced.one = with(ds1, str_count(runnerMovement, paste(start1B, ":1B:2B::", sep = "")))
    br1.advanced.two = with(ds1, str_count(runnerMovement, paste(start1B, ":1B:3B::", sep = "")))
    ds1 <- mutate_(ds1, basesAdvanced = ~ifelse(br1.scored == 1, 3, ifelse(br1.out == 1, -1, ifelse(br1.advanced.one == 1, 1, 
        ifelse(br1.advanced.two == 1, 2, 0)))))
    
    # Compute the number of bases advanced by each baserunner data$br0.adv = ifelse(br0.scored == 1, 4, ifelse(br0.advanced.one
    # == 1, 1, ifelse(br0.advanced.two == 1, 2, ifelse(br0.advanced.three == 1, 3, 0))))
    data[br1.idx, "br1.adv"] = ds1$basesAdvanced
    data[br2.idx, "br2.adv"] = ds2$basesAdvanced
    data[br3.idx, "br3.adv"] = ds3$basesAdvanced
    
    # Compute the empirical probabilities
    getCDF = function(ds) {
        # events = ddply(ds, ~basesAdvanced, summarise, N = length(basesAdvanced))
        events <- summarise_(group_by_(ds, ~basesAdvanced), N = ~length(basesAdvanced))
        events = mutate_(events, numObs = ~nrow(ds))
        events = mutate_(events, p = ~N/numObs)
        events$cdf = cumsum(events$p)
        events$cdf.lag = c(0, cumsum(events$p[-nrow(events)]))
        return(events)
    }
    
    # ds3Probs = plyr::ddply(ds3, ~event + startCode + startOuts, getCDF) ds2Probs = ddply(ds2, ~event + startCode + startOuts,
    # getCDF) ds1Probs = ddply(ds1, ~event + startCode + startOuts, getCDF)
    
    ds3Probs <- dplyr::do_(group_by_(ds3, ~event, ~startCode, ~startOuts), ~getCDF(.))
    ds2Probs <- dplyr::do_(group_by_(ds2, ~event, ~startCode, ~startOuts), ~getCDF(.))
    ds1Probs <- dplyr::do_(group_by_(ds1, ~event, ~startCode, ~startOuts), ~getCDF(.))
    
    # Merge onto the main data frame
    join.idx = c("event", "startCode", "startOuts")
    data = merge(x = data, y = ds3Probs[, c(join.idx, "basesAdvanced", "cdf.lag")], by.x = c(join.idx, "br3.adv"), by.y = c(join.idx, 
        "basesAdvanced"), all.x = TRUE)
    # Rename column
    data = dplyr::rename_(data, cdf.br3 = ~cdf.lag)
    
    data = merge(x = data, y = ds2Probs[, c(join.idx, "basesAdvanced", "cdf.lag")], by.x = c(join.idx, "br2.adv"), by.y = c(join.idx, 
        "basesAdvanced"), all.x = TRUE)
    data = dplyr::rename_(data, cdf.br2 = ~cdf.lag)
    data = merge(x = data, y = ds1Probs[, c(join.idx, "basesAdvanced", "cdf.lag")], by.x = c(join.idx, "br1.adv"), by.y = c(join.idx, 
        "basesAdvanced"), all.x = TRUE)
    data = dplyr::rename_(data, cdf.br1 = ~cdf.lag)
    
    # Make sure that baserunners who get out have a non-zero share
    data$cdf.br1[data$cdf.br1 == 0] <- 1e-08
    data$cdf.br2[data$cdf.br2 == 0] <- 1e-08
    data$cdf.br3[data$cdf.br3 == 0] <- 1e-08
    # Give zeros to the bases that were not occupied
    data$cdf.br1[is.na(data$cdf.br1)] <- 0
    data$cdf.br2[is.na(data$cdf.br2)] <- 0
    data$cdf.br3[is.na(data$cdf.br3)] <- 0
    
    # normalize the cdf probs
    data$share.br1 <- data$cdf.br1/(data$cdf.br1 + data$cdf.br2 + data$cdf.br3)
    data$share.br2 <- data$cdf.br2/(data$cdf.br1 + data$cdf.br2 + data$cdf.br3)
    data$share.br3 <- data$cdf.br3/(data$cdf.br1 + data$cdf.br2 + data$cdf.br3)
    
    # data$delta.br0 = with(data, ifelse(basesAdvanced == 0, 0, delta.br * (br0.extra / basesAdvanced)))
    data$delta.br[is.na(data$delta.br)] <- 0
    data$raa.br1 = data$share.br1 * data$delta.br
    data$raa.br2 = data$share.br2 * data$delta.br
    data$raa.br3 = data$share.br3 * data$delta.br
    
    return(data[, c("idx", "raa.br1", "raa.br2", "raa.br3")])
} 

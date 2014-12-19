#' @title makeWAR
#' @aliases makeWAR.GameDayPlays
#' 
#' @description Computes runs above average (RAA) for each player involved in each play of the "GameDayPlays" object. 
#' 
#' @details Within a "GameDayPlays" object, each row consists of a single plate appearance and contains information about the batter, all of the baserunners,
#'  the pitcher, and all of the fielders on the field during the plate appearance. The total value of the play as determined by the change in the run expectancy 
#'  matrix from the beginning of the plate appearence to the end of the plate appearance is partitioned across all players involved in with the play on offense, 
#'  and that same value (with the opposite sign) is partitioned across the pitcher and all of the fielders.  Thus for every single plate appearance a runs above average (RAA) value is assigned
#'  to every player involved in the play.  
#'  If no \code{models} argument is supplied, then all
#' models necessary for the computation of openWAR will be generated on the data set given. 
#' The output of this function is then used in the function getWAR to calculate a Wins Above Replacement (WAR) value for each player. 
#' 
#' If \code{verbose == TRUE}, then various pieces of information will be displayed during the comuptation.
#' 
#' Elements of \code{models}:
#' \itemize{
#' \item{run-expectancy}{: a model for assigning a run expectancy value to any of the 24 (base,out) states. Variables
#' must be "startCode" [0-7] and "startOuts" [0-2]}
#' \item{pitching}{: a model for the expected outcome of a plate appearance attributable to the pitcher. Variables
#' must be "venueId", "throws" [L/R], and "stands" [L/R]}
#' \item{offense}{: a model for the expected outcome of a plate appearance attributable to the offense. Variables
#' must be "venueId", "throws" [L/R], and "stands" [L/R]}
#' \item{baserunning}{: a model for the expected contribution of the baserunners to a plate appearance. Variables
#' must be "event" (the type of batting event), "startCode" [0-7], and "startOuts" [0-2]}
#' \item{batting}{: a model for the expected contribution of the batter to a plate appearance. Variables
#' must be "batterPos" (the defensive position of the batter)}
#' }
#' 
#' @param data An object of class "GameDayPlays"
#' @param models A named list of models, each with a predict() method. See Details.
#' @param verbose A LOGICAL indicating whether you want various messages and information to be displayed
#' during the computation
#' 
#' @return An object of class "openWARPlays" which is a list of length 4 containing the following: 
#' \itemize{
#' \item{plays}{A data.frame of class "GameDayPlays" that is the same as the input to the function.}
#' \item{data}{A data.frame of class "GameDayPlaysExt" containing the original data along with appended rows containing the RAA values for each player involved in a plate appearance}
#'  \item{models.used}{A list containing all of the model information for each of the models used in computing RAA.}
#'   \item{openWAR}{A data.frame of class "openWARPlays" containing only the columns necessry for input into the getWAR function.  }
#' }
#' 
#' 
#' @export makeWAR
#' @export makeWAR.GameDayPlays
#' @examples
#' 
#' # ds = getData(start = "2013-03-31", end = "2013-09-30")
#' res = makeWAR(ds)
#' 

makeWAR = function (data, models = list(), verbose = TRUE, ...) UseMethod("makeWAR")

makeWAR.GameDayPlays = function (data, models = list(), verbose = TRUE, low.memory = TRUE, ...) {
  orig = data
  data$idx = 1:nrow(data)
  ###########################################################################################
  # Step 1: Define \delta, the change in expected runs
  mod.re = getModelRunExpectancy(data[, c("outsInInning", "runsFuture", "startCode", "startOuts")]
                    , models[["run-expectancy"]], verbose)
  models.used = list("run-expectancy" = mod.re)
  deltas = makeWARre24(data[, c("startCode", "startOuts", "endCode", "endOuts", "runsOnPlay")], mod.re, verbose)
  data = cbind(data, deltas)
  
  ###########################################################################################
  # Step 2: Define RAA for the defense
  # Work only with the subset of data for which the ball is in play and keep track of the indices
  bip.idx = which(data$isBIP == TRUE)
  ds.field = data[bip.idx,]
  fielding = makeWARFielding(ds.field, models, verbose)
  
  for(col.name in names(fielding)) {
    data[bip.idx, col.name] = fielding[,col.name]
  }
  
  ###########################################################################################
  # Step 3: Define RAA for the pitcher
  data$delta.pitch = with(data, ifelse(is.na(delta.field), delta, delta - delta.field))
  message("...Estimating Pitching Runs Above Average...")
  mod.pitch = getModelPitching(data[,c("delta.pitch", "venueId", "throws", "stand")], models[["pitching"]], verbose)
  if (verbose) {
    message("....Pitching Model....")
    print(sort(coef(mod.pitch)))
  }
  
  models.used[["pitching"]] = mod.pitch
  # Note the pitcher RAA's are the negative residuals!
  data$raa.pitch = predict(mod.pitch, newdata=data[,c("venueId", "throws", "stand")]) - data$delta.pitch  
  
  ###########################################################################################
  # Step 4: Define RAA for the batter
  message("...Building model for offense...")
  mod.off = getModelOffense(data[,c("delta", "venueId", "throws", "stand")], models[["offense"]], verbose)
  if (verbose) {
    message("....Offense Model....")
    print(sort(coef(mod.off)))
  }
  models.used[["offense"]] = mod.off
  # delta.off is the contribution above average of the batter AND all of the runners
  data$delta.off = data$delta - predict(mod.off, newdata=data[,c("venueId", "throws", "stand")])
  
  # If runners are on base, partition delta between the batter and baserunners
  br.idx = which(data$startCode > 0)
  message("...Partitioning Offense into Batting and Baserunning...")
  mod.br = getModelBaserunning(data[br.idx, c("delta.off", "event", "startCode", "startOuts")], models[["baserunning"]], verbose)
  if (verbose) {
    message("....Baserunning Model....")
    message(paste("....", length(coef(mod.br)), "coefficients -- suppressing output..."))
    #    print(sort(coef(mod.br)))
  }
  models.used[["baserunning"]] = mod.br
  # delta.br is the contribution of the baserunners beyond the event type
  data[br.idx, "delta.br"] = data[br.idx, "delta.off"] - predict(mod.br, newdata=data[br.idx, c("event", "startCode", "startOuts")])
  
  # Whatever is left over goes to the batter -- just control for defensive position
  data$delta.bat = with(data, ifelse(is.na(delta.br), delta, delta - delta.br))
  message("...Estimating Batting Runs Above Average...")
  mod.bat = getModelBatting(data[, c("delta.bat", "batterPos")], models[["batting"]], verbose)
  if (verbose) {
    message("....Batting Model....")
    print(sort(coef(mod.bat)))
  }
  models.used[["batting"]] = mod.bat
  # Control for batter position
  # Note that including "idx" is not necessary -- it just ensure that the argument passed is a data.frame
  data$raa.bat = data$delta.bat - predict(mod.bat, newdata=data[, c("batterPos", "idx")])

  
  ###########################################################################################
  # Step 5: Define RAA for the baserunners
  br.fields = c("idx", "delta.br", "start1B", "start2B", "start3B", "end1B", "end2B", "end3B"
                , "runnerMovement", "event", "startCode", "startOuts")
  raa.br = makeWARBaserunning(data[br.idx, br.fields], models[["baserunning"]], verbose)
  data = merge(x=data, y=raa.br, by="idx", all.x=TRUE)
  
  ###########################################################################################
  # Add the new class
  class(data) = c("GameDayPlaysExt", "GameDayPlays", "data.frame")
  # include the computations as a separate data.frame
  id.fields = c("batterId", "start1B", "start2B", "start3B", "pitcherId", "playerId.C", "playerId.1B"
                , "playerId.2B", "playerId.3B", "playerId.SS", "playerId.LF", "playerId.CF", "playerId.RF"
                , "batterName", "pitcherName", "gameId", "event", "isPA")
  delta.fields = c("delta", "delta.field", "delta.pitch", "delta.br", "delta.bat")
  raa.fields = c("raa.bat", "raa.br1", "raa.br2", "raa.br3", "raa.pitch", "raa.P", "raa.C", "raa.1B"
                 , "raa.2B", "raa.3B", "raa.SS", "raa.LF", "raa.CF", "raa.RF")
  openWARPlays = data[, c(id.fields, delta.fields, raa.fields)]
  class(openWARPlays) = c("openWARPlays", "data.frame")
  if (low.memory) {
    return(list("plays" = NULL, "data" = NULL, "models.used" = NULL, "openWAR" = openWARPlays))
  } else {
    return(list("plays" = orig, "data" = data, "models.used" = models.used, "openWAR" = openWARPlays))
  }
}


makeWARre24 = function (data, mod.re = NULL, verbose=TRUE, ...) {
  message("...Estimating Expected Runs...")
  
  begin.states = data[, c("startCode", "startOuts")]
  end.states = data[, c("endCode", "endOuts")]
  end.states$endOuts = with(end.states, ifelse(endOuts == 3, NA, endOuts))
  names(end.states) = names(begin.states)
  
  startExR = predict(mod.re, newdata=begin.states)
  endExR = predict(mod.re, newdata=end.states)
  endExR = ifelse(is.na(endExR), 0, endExR)

  out = data.frame(startExR, endExR)
  out$delta = endExR - startExR + data$runsOnPlay
  return(out)
}


makeWARFielding = function (data, models = list(), verbose=TRUE, ...) {
  message("...Estimating Fielding Runs Above Average...")
  
  data = transform(data, wasFielded = !is.na(fielderId))
  # Compute the collective responsibility of all fielders
  p.hat = getModelFieldingCollective(data[, c("wasFielded", "our.x", "our.y")])
  # Step 2a: Define \delta.field for the defense, collectively
  delta.field = data$delta * p.hat
  
  # Compute the individual responsibility of each fielder
  P = getFielderResp(data)
  # Step 2b: Define \delta.field for the defense, individually
  delta.fielders = delta.field * P
  names(delta.fielders) = gsub("resp", "delta", names(delta.fielders))
  
  out = data.frame(p.hat, delta.field, delta.fielders)
  
  # Normalize the delta's into RAA's
  raa.field = getFielderRAA(cbind(out, venueId = data$venueId))
  return(cbind(out, raa.field))
}


#' 
#' @title getFielderRAA
#' 
#' @description Determine the Runs Above Average (RAA) of the fielders
#' 
#' @details RAA is the residuals from a simple fielding model.  Used in the function \code{makeWARFielding}
#' 
#' @param data A data.frame containg an estimate of the probably that a ball on a given play would be fielded, the total delta for fielders, a column of delta for each fielder, and a venueId
#' 
#' @return A data.frame containing fielding RAA values for all plate appearances with a ball in play
#' 
#' @export
#' 
#' 

getFielderRAA = function (data) {
  # Build a model for each fielder's expected change in runs
  mod.P = lm(delta.P ~ factor(venueId), data = data)
  mod.C = lm(delta.C ~ factor(venueId), data = data)
  mod.1B = lm(delta.1B ~ factor(venueId), data = data)
  mod.2B = lm(delta.2B ~ factor(venueId), data = data)
  mod.3B = lm(delta.3B ~ factor(venueId), data = data)
  mod.SS = lm(delta.SS ~ factor(venueId), data = data)
  mod.LF = lm(delta.LF ~ factor(venueId), data = data)
  mod.CF = lm(delta.CF ~ factor(venueId), data = data)
  mod.RF = lm(delta.RF ~ factor(venueId), data = data)
  
  # Define RAA to be the residuals from the individual fielders models
  raa = -data.frame(mod.P$residuals, mod.C$residuals, mod.1B$residuals, mod.2B$residuals, mod.3B$residuals
                    , mod.SS$residuals, mod.LF$residuals, mod.CF$residuals, mod.RF$residuals)
  names(raa) = gsub("mod", "raa", gsub(".residuals", "", names(raa)))
  
  # The column-wise sums should all be zero
  #  colSums(raa)
  return(raa)
}




#' @title getFielderResp
#' 
#' @description Find the shared responsibility for balls in play
#' 
#' @details Fits 9 logistic regression models, each giving the probability of 
#' a fielder at one of the 9 defensive positions successfully converting the 
#' ball into at least one out.
#' 
#' @param data An MLBAM data.frame 
#' 
#' @return data.frame with 9 columns, each row representing a ball in play
#' 
#' @export
#' 


getFielderResp = function (data, ...) {  
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
  
  #   mod = mod.CF
  #   summary(mod)
  #   fit = makeFun(mod)
  #   plotFun(fit(x,y) ~ x + y, surface=TRUE, alpha=0.9
  #           , xlim = c(-350, 350), ylim = c(0, 550)
  #           , xlab = "Horizontal Distance from Home Plate (ft.)"
  #           , ylab = "Vertical Distance from Home Plate (ft.)"
  #           , zlab = "Probability of Making a Play"
  #   )
  
  out = data.frame(mod.P$fitted, mod.C$fitted, mod.1B$fitted, mod.2B$fitted, mod.3B$fitted
                   , mod.SS$fitted, mod.LF$fitted, mod.CF$fitted, mod.RF$fitted)
  row.sums = apply(out, 1, sum)
  out = out / row.sums
  names(out) = c("resp.P", "resp.C", "resp.1B", "resp.2B", "resp.3B", "resp.SS", "resp.LF", "resp.CF", "resp.RF")
  return(out)
}


makeWARBaserunning = function (data, mod.bat, verbose = TRUE, ...) {
  message("...Estimating Baserunning Runs Above Average...")
  
#  require(plyr)
  require(MASS)
  require(stringr)
  
  # Figure out what happened to the runner on 3B
  data <- mutate(data, dest.br3 = ifelse(str_count(runnerMovement, paste(start3B, ":3B::T:", sep="")), "H", NA))
  data <- mutate(data, dest.br3 = ifelse(!is.na(start3B) & !is.na(end3B) & start3B == end3B, "3B", dest.br3))
  
  br3.idx = which(!is.na(data$start3B))
  ds3 = data[br3.idx,]
  br3.scored = with(ds3, str_count(runnerMovement, paste(start3B, ":3B::T:", sep="")))
  br3.out = with(ds3, str_count(runnerMovement, paste(start3B, ":3B:::", sep="")))
  ds3 <- mutate(ds3, basesAdvanced = ifelse(br3.scored == 1, 1, ifelse(br3.out == 1, -3, 0)))
  
  # Figure out what happened to the runner on 2B
  data <- mutate(data, dest.br2 = ifelse(str_count(runnerMovement, paste(start2B, ":2B::T:", sep="")), "H", NA))
  data <- mutate(data, dest.br2 = ifelse(!is.na(start2B) & !is.na(end3B) & start2B == end3B, "3B", dest.br2))
  data <- mutate(data, dest.br2 = ifelse(!is.na(start2B) & !is.na(end2B) & start2B == end2B, "2B", dest.br2))
  
  br2.idx = which(!is.na(data$start2B))
  ds2 = data[br2.idx,]
  br2.scored = with(ds2, str_count(runnerMovement, paste(start2B, ":2B::T:", sep="")))
  br2.out = with(ds2, str_count(runnerMovement, paste(start2B, ":2B:::", sep="")))
  br2.advanced = with(ds2, str_count(runnerMovement, paste(start2B, ":2B:3B::", sep="")))
  ds2 <- mutate(ds2, basesAdvanced = ifelse(br2.scored == 1, 2, ifelse(br2.out == 1, -2, ifelse(br2.advanced == 1, 1, 0))))
  
  # Figure out what happened to the runner on 1B
  data <- mutate(data, dest.br1 = ifelse(str_count(runnerMovement, paste(start1B, ":1B::T:", sep="")), "H", NA))
  data <- mutate(data, dest.br1 = ifelse(!is.na(start1B) & !is.na(end3B) & start1B == end3B, "3B", dest.br1))
  data <- mutate(data, dest.br1 = ifelse(!is.na(start1B) & !is.na(end2B) & start1B == end2B, "2B", dest.br1))
  data <- mutate(data, dest.br1 = ifelse(!is.na(start1B) & !is.na(end1B) & start1B == end1B, "1B", dest.br1))
  
  br1.idx = which(!is.na(data$start1B))
  ds1 = data[br1.idx,]
  br1.scored = with(ds1, str_count(runnerMovement, paste(start1B, ":1B::T:", sep="")))
  br1.out = with(ds1, str_count(runnerMovement, paste(start1B, ":1B:::", sep="")))
  br1.advanced.one = with(ds1, str_count(runnerMovement, paste(start1B, ":1B:2B::", sep="")))
  br1.advanced.two = with(ds1, str_count(runnerMovement, paste(start1B, ":1B:3B::", sep="")))
  ds1 <- mutate(ds1, basesAdvanced = ifelse(br1.scored == 1, 3, ifelse(br1.out == 1, -1, ifelse(br1.advanced.one == 1, 1, ifelse(br1.advanced.two == 1, 2, 0)))))
  
  # Compute the number of bases advanced by each baserunner
  # data$br0.adv = ifelse(br0.scored == 1, 4, ifelse(br0.advanced.one == 1, 1, ifelse(br0.advanced.two == 1, 2, ifelse(br0.advanced.three == 1, 3, 0))))
  data[br1.idx, "br1.adv"] = ds1$basesAdvanced
  data[br2.idx, "br2.adv"] = ds2$basesAdvanced
  data[br3.idx, "br3.adv"] = ds3$basesAdvanced
  
  # Compute the empirical probabilities
  getCDF = function (ds) {
    # events = ddply(ds, ~basesAdvanced, summarise, N = length(basesAdvanced))
    events <- summarise(group_by(ds, basesAdvanced), N = length(basesAdvanced))
    events = mutate(events, numObs = nrow(ds))
    events = mutate(events, p = N / numObs)
    events$cdf = cumsum(events$p)
    events$cdf.lag = c(0, cumsum(events$p[-nrow(events)]))
    return(events)
  }
  
#  ds3Probs = plyr::ddply(ds3, ~event + startCode + startOuts, getCDF)
#  ds2Probs = ddply(ds2, ~event + startCode + startOuts, getCDF)
#  ds1Probs = ddply(ds1, ~event + startCode + startOuts, getCDF)

  ds3Probs <- do(group_by(ds3, event, startCode, startOuts), getCDF(.))
  ds2Probs <- do(group_by(ds2, event, startCode, startOuts), getCDF(.))
  ds1Probs <- do(group_by(ds1, event, startCode, startOuts), getCDF(.))

  # Merge onto the main data frame
  join.idx = c("event", "startCode", "startOuts")
  data = merge(x = data, y = ds3Probs[,c(join.idx, "basesAdvanced", "cdf.lag")], by.x = c(join.idx, "br3.adv"), by.y = c(join.idx, "basesAdvanced"), all.x=TRUE)
  # Rename column
  data = rename(data, cdf.br3 = cdf.lag)
  
  data = merge(x = data, y = ds2Probs[,c(join.idx, "basesAdvanced", "cdf.lag")], by.x = c(join.idx, "br2.adv"), by.y = c(join.idx, "basesAdvanced"), all.x=TRUE)
  data = rename(data, cdf.br2 = cdf.lag)
  data = merge(x = data, y = ds1Probs[,c(join.idx, "basesAdvanced", "cdf.lag")], by.x = c(join.idx, "br1.adv"), by.y = c(join.idx, "basesAdvanced"), all.x=TRUE)
  data = rename(data, cdf.br1 = cdf.lag)
  
  # Make sure that baserunners who get out have a non-zero share
  data$cdf.br1[data$cdf.br1 == 0] <- 0.00000001
  data$cdf.br2[data$cdf.br2 == 0] <- 0.00000001
  data$cdf.br3[data$cdf.br3 == 0] <- 0.00000001
  # Give zeros to the bases that were not occupied
  data$cdf.br1[is.na(data$cdf.br1)] <- 0
  data$cdf.br2[is.na(data$cdf.br2)] <- 0
  data$cdf.br3[is.na(data$cdf.br3)] <- 0
  
  #normalize the cdf probs
  data$share.br1 <- data$cdf.br1 / (data$cdf.br1 + data$cdf.br2 + data$cdf.br3)
  data$share.br2 <- data$cdf.br2 / (data$cdf.br1 + data$cdf.br2 + data$cdf.br3)
  data$share.br3 <- data$cdf.br3 / (data$cdf.br1 + data$cdf.br2 + data$cdf.br3)
  
  #  data$delta.br0 = with(data, ifelse(basesAdvanced == 0, 0, delta.br * (br0.extra / basesAdvanced)))
  data$delta.br[is.na(data$delta.br)] <- 0
  data$raa.br1 = data$share.br1 * data$delta.br
  data$raa.br2 = data$share.br2 * data$delta.br
  data$raa.br3 = data$share.br3 * data$delta.br
  
  return(data[, c("idx", "raa.br1", "raa.br2", "raa.br3")])
}


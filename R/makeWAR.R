#' @title makeWAR
#' @aliases makeWAR.GameDayPlays
#' 
#' @description Compute openWAR
#' 
#' @details Computes openWAR, given an MLBAM data set
#' 
#' @param data A GameDayPlays data set
#' @param re.mod A run expectancy model with a predict() method. Expects to take the two variables 
#' startCode and startOuts
#' @param verbose A LOGICAL indicating whether you want various messages and information to be displayed
#' during the computation
#' 
#' @return a GameDayPlaysExt data.frame
#' 
#' @export makeWAR
#' @export makeWAR.GameDayPlays
#' @examples
#' 
#' ds = getData()
#' res = makeWAR(ds)
#' 

makeWAR = function (data, re.mod = NULL, verbose = TRUE, ...) UseMethod("makeWAR")

makeWAR.GameDayPlays = function (data, re.mod = NULL, verbose = FALSE, ...) {
  # Step 1: Define \delta, the change in expected runs
  message("...Estimating Expected Runs...")
  
  # Check to see whether the supplied run expectancy model has a predict() method
  if (!paste("predict", class(re.mod), sep=".") %in% methods(predict)) {
    message("....Supplied Run Expectancy model does not have a predict method...")
    message("....Building in-sample Run Expectancy Model...")
    re.mod = getModel(data, type = "run-expectancy")
  }
  
  if (verbose) {
    message("....Run Expectancy Model....")
    states = expand.grid(startCode = 0:7, startOuts = 0:2)
    print(matrix(predict(re.mod, newdata=states), ncol=3))
  }
   
  begin.states = data[,c("startCode", "startOuts")]
  end.states = data[,c("endCode", "endOuts")]
  end.states$endOuts = with(end.states, ifelse(endOuts == 3, NA, endOuts))
  names(end.states) = names(begin.states)
  
  data = transform(data, startExR = predict(re.mod, newdata=begin.states))
  data = transform(data, endExR = predict(re.mod, newdata=end.states))
  data$endExR = with(data, ifelse(is.na(endExR), 0, endExR))

  # The old way
#  fit.rem = getRunEx(data)
#  data = transform(data, startExR = fit.rem(startCode, startOuts))
#  data = transform(data, endExR = fit.rem(endCode, endOuts))
  data = transform(data, delta = endExR - startExR + runsOnPlay)
  
  # Step 2: Define RAA for the defense
  message("...Estimating Fielding Runs Above Average...")
  # Work only with the subset of data for which the ball is in play and keep track of the indices
  bip.idx = which(data$isBIP == TRUE)
  ds.field = data[bip.idx,]
  ds.field = getFielderRAA(ds.field)
  new.names = names(ds.field)[!names(ds.field) %in% names(data)]
  for(col.name in new.names) {
    data[bip.idx, col.name] = ds.field[,col.name]
  }
  
  # Step 3: Define RAA for the pitcher
  message("...Estimating Pitching Runs Above Average...")
  data$delta.pitch = with(data, ifelse(is.na(delta.field), delta, delta - delta.field))
  mod.pitch = lm(delta.pitch ~ stadium + (throws == stand), data = data)
  #  summary(mod.pitch)
  data$raa.pitch = -mod.pitch$residuals
  
  # Step 4: Define RAA for the batter
  message("...Estimating Batting Runs Above Average...")
  require(plyr)
  
#   if ( method == "ghostrunner") {
#     # Figure out the most common outcome for every beginning state and event type
#     getMostCommon = function(df) {
#       outcomes = ddply(df, ~endCode + endOuts + runsOnPlay, summarise, N = length(endCode))
#       outcomes$Pct = outcomes$N / nrow(df)
#       names(outcomes)[which(names(outcomes) %in% c("endCode", "endOuts", "runsOnPlay"))] = c("endBatCode", "endBatOuts", "batRunsOnPlay")
#       return(outcomes[which.max(outcomes$N),])
#     }
#     event.lkup = ddply(data, ~startCode + startOuts + event, getMostCommon)
#     #  densityplot(~Pct, data=event.lkup)
#     event.lkup = transform(event.lkup, bat.ExR = fit.rem(endBatCode, endBatOuts) + batRunsOnPlay)
#     data = merge(x=data, y=event.lkup[, c("startCode", "startOuts", "event", "endBatCode", "endBatOuts", "batRunsOnPlay", "bat.ExR")]
#                  , by = c("startCode", "startOuts", "event"), all.x=TRUE)
#     
#     # Assign that difference to the batter
#     data = transform(data, delta.bat = bat.ExR - startExR)     
#     mod.bat = lm(delta.bat ~ as.factor(batterPos) + stadium + (stand == throws), data=data)
#     #  summary(mod.bat)
#     data = transform(data, raa.bat = mod.bat$residuals)   
#   } else {
    # Control for circumstances
    mod.off = lm(delta ~ stadium + (stand == throws), data=data)
    # summary(mod.off)
    # delta.off is the contribution above average of the batter AND all of the runners
    data = transform(data, delta.off = mod.off$residuals)  
    # Siphon off the portion attributable to the baserunners
    br.idx = which(data$startCode > 0)
    mod.br = lm(delta.off ~ event * as.factor(startCode) * as.factor(startOuts), data=data[br.idx,])
    data[br.idx, "delta.br"] = mod.br$residuals
    # Whatever is left over goes to the batter
    data$delta.bat = with(data, ifelse(is.na(delta.br), delta, delta - delta.br))
    # Control for defensive position
    mod.bat = lm(delta.bat ~ as.factor(batterPos), data=data)
    data = transform(data, raa.bat = mod.bat$residuals)
#   }
  
  if (verbose) {
    print(sort(coef(mod.bat)))
  }
  
  # Step 5: Define RAA for the baserunners
  message("...Estimating Baserunning Runs Above Average...")
  
  require(MASS)
  require(stringr)
  # Figure out what happened to the runner on 3B
  data$dest.br3 = with(data, ifelse(str_count(runnerMovement, paste(start3B, ":3B::T:", sep="")), "H", NA))
  data$dest.br3 = with(data, ifelse(!is.na(start3B) & !is.na(end3B) & start3B == end3B, "3B", dest.br3))
  
  br3.idx = which(!is.na(data$start3B))
  ds3 = data[br3.idx,]
  br3.scored = with(ds3, str_count(runnerMovement, paste(start3B, ":3B::T:", sep="")))
  br3.out = with(ds3, str_count(runnerMovement, paste(start3B, ":3B:::", sep="")))
  ds3$basesAdvanced = ifelse(br3.scored == 1, 1, ifelse(br3.out == 1, -3, 0))
  
  # Figure out what happened to the runner on 2B
  data$dest.br2 = with(data, ifelse(str_count(runnerMovement, paste(start2B, ":2B::T:", sep="")), "H", NA))
  data$dest.br2 = with(data, ifelse(!is.na(start2B) & !is.na(end3B) & start2B == end3B, "3B", dest.br2))
  data$dest.br2 = with(data, ifelse(!is.na(start2B) & !is.na(end2B) & start2B == end2B, "2B", dest.br2))
  
  br2.idx = which(!is.na(data$start2B))
  ds2 = data[br2.idx,]
  br2.scored = with(ds2, str_count(runnerMovement, paste(start2B, ":2B::T:", sep="")))
  br2.out = with(ds2, str_count(runnerMovement, paste(start2B, ":2B:::", sep="")))
  br2.advanced = with(ds2, str_count(runnerMovement, paste(start2B, ":2B:3B::", sep="")))
  ds2$basesAdvanced = ifelse(br2.scored == 1, 2, ifelse(br2.out == 1, -2, ifelse(br2.advanced == 1, 1, 0)))
  
  # Figure out what happened to the runner on 1B
  data$dest.br1 = with(data, ifelse(str_count(runnerMovement, paste(start1B, ":1B::T:", sep="")), "H", NA))
  data$dest.br1 = with(data, ifelse(!is.na(start1B) & !is.na(end3B) & start1B == end3B, "3B", dest.br1))
  data$dest.br1 = with(data, ifelse(!is.na(start1B) & !is.na(end2B) & start1B == end2B, "2B", dest.br1))
  data$dest.br1 = with(data, ifelse(!is.na(start1B) & !is.na(end1B) & start1B == end1B, "1B", dest.br1))
  
  br1.idx = which(!is.na(data$start1B))
  ds1 = data[br1.idx,]
  br1.scored = with(ds1, str_count(runnerMovement, paste(start1B, ":1B::T:", sep="")))
  br1.out = with(ds1, str_count(runnerMovement, paste(start1B, ":1B:::", sep="")))
  br1.advanced.one = with(ds1, str_count(runnerMovement, paste(start1B, ":1B:2B::", sep="")))
  br1.advanced.two = with(ds1, str_count(runnerMovement, paste(start1B, ":1B:3B::", sep="")))
  ds1$basesAdvanced = ifelse(br1.scored == 1, 3, ifelse(br1.out == 1, -1, ifelse(br1.advanced.one == 1, 1, ifelse(br1.advanced.two == 1, 2, 0))))
  
  # Compute the number of bases advanced by each baserunner
  # data$br0.adv = ifelse(br0.scored == 1, 4, ifelse(br0.advanced.one == 1, 1, ifelse(br0.advanced.two == 1, 2, ifelse(br0.advanced.three == 1, 3, 0))))
  data[br1.idx, "br1.adv"] = ds1$basesAdvanced
  data[br2.idx, "br2.adv"] = ds2$basesAdvanced
  data[br3.idx, "br3.adv"] = ds3$basesAdvanced
  
  # Compute the empirical probabilities
  getCDF = function (ds) {
    events = ddply(ds, ~basesAdvanced, summarise, N = length(basesAdvanced))
    events = transform(events, numObs = nrow(ds))
    events = transform(events, p = N / numObs)
    events$cdf = cumsum(events$p)
    events$cdf.lag = c(0, cumsum(events$p[-nrow(events)]))
    return(events)
  }
  
  ds3Probs = ddply(ds3, ~event + startCode + startOuts, getCDF)
  ds2Probs = ddply(ds2, ~event + startCode + startOuts, getCDF)
  ds1Probs = ddply(ds1, ~event + startCode + startOuts, getCDF)
  
  # Merge onto the main data frame
  join.idx = c("event", "startCode", "startOuts")
  data = merge(x = data, y = ds3Probs[,c(join.idx, "basesAdvanced", "cdf.lag")], by.x = c(join.idx, "br3.adv"), by.y = c(join.idx, "basesAdvanced"), all.x=TRUE)
  # Rename column
  data = rename(data, c("cdf.lag" = "cdf.br3"))

  data = merge(x = data, y = ds2Probs[,c(join.idx, "basesAdvanced", "cdf.lag")], by.x = c(join.idx, "br2.adv"), by.y = c(join.idx, "basesAdvanced"), all.x=TRUE)
  data = rename(data, c("cdf.lag" = "cdf.br2"))
  data = merge(x = data, y = ds1Probs[,c(join.idx, "basesAdvanced", "cdf.lag")], by.x = c(join.idx, "br1.adv"), by.y = c(join.idx, "basesAdvanced"), all.x=TRUE)
  data = rename(data, c("cdf.lag" = "cdf.br1"))
  
  # Compute a share for each baserunner
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
  
  #  mod.br3 = lm(basesAdvanced ~ event * as.factor(startOuts), data = ds3)
  #  mod.br2 = lm(basesAdvanced ~ event * as.factor(startOuts), data = ds2)
  #  mod.br1 = lm(basesAdvanced ~ event * as.factor(startOuts), data = ds1)
  #  mod.br0 = lm(br0.adv ~ event * as.factor(startOuts), data = data)
  #  bwplot(mod.br3$resid ~ event, data=ds3)
  #  bwplot(mod.br2$resid ~ event, data=ds2)
  
  #mod.br3 = lm(delta.br3 ~ event * as.factor(startOuts), data = data)
  #mod.br2 = lm(delta.br2 ~ event * as.factor(startOuts), data = data)
  #mod.br1 = lm(delta.br1 ~ event * as.factor(startOuts), data = data)
  #  mod.br0 = lm(delta.br0 ~ event + as.factor(startOuts), data = data)
  
  # Placeholder in case we want to use this later on
  #  data$raa.br0 = mod.br0$residuals
  #data$raa.br0 = 0
  #data[!is.na(data$delta.br3), "raa.br3"] = mod.br3$residuals
  #data[!is.na(data$delta.br2), "raa.br2"] = mod.br2$residuals
  #data[!is.na(data$delta.br1), "raa.br1"] = mod.br1$residuals
  
  # Add the new class
  class(data) = c("GameDayPlaysExt", "GameDayPlays", "data.frame")
  return(data)
}


#' 
#' @title getFielderRAA
#' 
#' @description Determine the RAA of the fielders
#' 
#' @details RAA is the residuals from a simple fielding model
#' 
#' @param data An MLBAM data.frame of BIPs
#' 
#' @return a matrix of Runs Above Average (RAA)
#' 
#' @export
#' @examples
#' 
#' ds = getData()
#' ds$resp.field = getFieldResp(ds)
#' 

getFielderRAA = function (data) {
  # Compute the collective responsibility of all fielders
  data$resp.field = getFieldResp(data)
  # Compute the individual responsibility of each fielder
  resp.fielders = getFielderResp(data)
  
  # Step 2a: Define \delta.field for the defense, collectively
  data$delta.field = with(data, delta * resp.field)
  # Step 2b: Define \delta.field for the defense, individually
  delta.fielders = data$delta.field * resp.fielders
  names(delta.fielders) = gsub("resp", "delta", names(delta.fielders))
  data = cbind(data, delta.fielders)
  
  # Build a model for each fielder's expected change in runs
  mod.P = lm(delta.P ~ stadium, data = data)
  mod.C = lm(delta.C ~ stadium, data = data)
  mod.1B = lm(delta.1B ~ stadium, data = data)
  mod.2B = lm(delta.2B ~ stadium, data = data)
  mod.3B = lm(delta.3B ~ stadium, data = data)
  mod.SS = lm(delta.SS ~ stadium, data = data)
  mod.LF = lm(delta.LF ~ stadium, data = data)
  mod.CF = lm(delta.CF ~ stadium, data = data)
  mod.RF = lm(delta.RF ~ stadium, data = data)
  
  # Define RAA to be the residuals from the individual fielders models
  raa = -data.frame(mod.P$residuals, mod.C$residuals, mod.1B$residuals, mod.2B$residuals, mod.3B$residuals
                    , mod.SS$residuals, mod.LF$residuals, mod.CF$residuals, mod.RF$residuals)
  names(raa) = gsub("mod", "raa", gsub(".residuals", "", names(raa)))
  
  # The column-wise sums should all be zero
  #  colSums(raa)
  data = cbind(data, raa)
  return(data)
}



#' 
#' @title getFieldResp
#' 
#' @description Determine the responsibility of the fielders
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
#' ds$resp.field = getFieldResp(ds)
#' 

getFieldResp = function (data) {
  require(KernSmooth)
  outs = subset(data, !is.na(fielderId), select=c("our.x", "our.y"))
  hits = subset(data, is.na(fielderId), select=c("our.x", "our.y"))
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
  # wireframe(wasFielded ~ x + y, data=field.smooth, scales = list(arrows = FALSE), drape = TRUE, colorkey = TRUE)
  
  fit.all = function (x, y) {
    require(Hmisc)
    x.idx = whichClosest(field.smooth$x, x)
    y.idx = whichClosest(field.smooth$y, y)
    match = subset(field.smooth, x == field.smooth$x[x.idx] & y == field.smooth$y[y.idx])
    return(match$wasFielded)
  }
  
  resp.field = mapply(fit.all, data$our.x, data$our.y)
  return(resp.field)
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
#' @examples
#' 
#' ds = getData()
#' setBIPresp(ds)

getFielderResp = function (data, ...) {
  require(mosaic)
  ds = transform(data, wasFielded = !is.na(fielderId))
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
  
  mod.P = glm((fielderPos == "P") ~ poly(our.x, 2) + poly(our.y, 2) + I(our.x * our.y), data=ds, family="binomial")
  mod.C = glm((fielderPos == "C") ~ poly(our.x, 2) + poly(our.y, 2) + I(our.x * our.y), data=ds, family="binomial")
  mod.1B = glm((fielderPos == "1B") ~ poly(our.x, 2) + poly(our.y, 2), data=ds, family="binomial")
  mod.2B = glm((fielderPos == "2B") ~ poly(our.x, 2) + poly(our.y, 2) + I(our.x * our.y), data=ds, family="binomial")
  mod.3B = glm((fielderPos == "3B") ~ poly(our.x, 2) + poly(our.y, 2) + I(our.x * our.y), data=ds, family="binomial")
  mod.SS = glm((fielderPos == "SS") ~ poly(our.x, 2) + poly(our.y, 2) + I(our.x * our.y), data=ds, family="binomial")
  mod.LF = glm((fielderPos == "LF") ~ poly(our.x, 2) + poly(our.y, 2) + I(our.x * our.y), data=ds, family="binomial")
  mod.CF = glm((fielderPos == "CF") ~ poly(our.x, 2) + poly(our.y, 2) + I(our.x * our.y), data=ds, family="binomial")
  mod.RF = glm((fielderPos == "RF") ~ poly(our.x, 2) + poly(our.y, 2) + I(our.x * our.y), data=ds, family="binomial")
  
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

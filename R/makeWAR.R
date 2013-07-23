#' @title makeWAR
#' 
#' @description Compute openWAR
#' 
#' @details Computes openWAR, given an MLBAM data set
#' 
#' @param data An MLBAM data.frame 
#' 
#' @return a data.frame
#' 
#' @export
#' @examples
#' 
#' ds = getData()
#' res = makeWAR(ds)
#' 

makeWAR = function (data, ...) {
  # Step 1: Define \delta, the change in expected runs
  message("...Estimating Expected Runs...")
  fit.rem = getRunEx(data)
  data = transform(data, startExR = fit.rem(startCode, startOuts))
  data = transform(data, endExR = fit.rem(endCode, endOuts))
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
  
  # Step 4: Define RAA for the baserunners
  message("...Estimating Baserunning Runs Above Average...")
  require(MASS)
  require(stringr)
  # Figure out what happened to the runner on 3B
  br3.idx = which(!is.na(data$start3B))
  ds3 = data[br3.idx,]
  scored3B = str_count(ds3$runnerMovement, paste(ds3$start3B, ":3B::T:", sep=""))
  out3B = str_count(ds3$runnerMovement, paste(ds3$start3B, ":3B:::", sep=""))
  basesAdvanced = ifelse(scored3B == 1, 1, ifelse(out3B == 1, -3, 0))
  endCode.br3 = ifelse(basesAdvanced == 0, 4, 0) 
  endOuts.br3 = with(ds3, ifelse(basesAdvanced == -3, startOuts + 1, startOuts)) 
  ds3 = transform(ds3, delta.br3 = fit.rem(endCode.br3, endOuts.br3) - fit.rem(rep(4, nrow(ds3)), startOuts) + (basesAdvanced == 1))
  
  # Figure out what happened to the runner on 2B
  br2.idx = which(!is.na(data$start2B))
  ds2 = data[br2.idx,]
  scored2B = with(ds2, str_count(runnerMovement, paste(start2B, ":2B::T:", sep="")))
  out2B = with(ds2, str_count(runnerMovement, paste(start2B, ":2B:::", sep="")))
  advanced = with(ds2, str_count(runnerMovement, paste(start2B, ":2B:3B::", sep="")))
  basesAdvanced = ifelse(scored2B == 1, 2, ifelse(out2B == 1, -2, ifelse(advanced == 1, 1, 0)))
  endCode.br2 = ifelse(basesAdvanced == 0, 2, ifelse(basesAdvanced == 1, 4, 0)) 
  endOuts.br2 = with(ds2, ifelse(basesAdvanced == -2, startOuts + 1, startOuts)) 
  ds2 = transform(ds2, delta.br2 = fit.rem(endCode.br2, endOuts.br2) - fit.rem(rep(2, nrow(ds2)), startOuts) + (basesAdvanced == 2))
  
  # Figure out what happened to the runner on 1B
  br1.idx = which(!is.na(data$start1B))
  ds1 = data[br1.idx,]
  scored1B = with(ds1, str_count(runnerMovement, paste(start1B, ":1B::T:", sep="")))
  out1B = with(ds1, str_count(runnerMovement, paste(start1B, ":1B:::", sep="")))
  advanced.one = with(ds1, str_count(runnerMovement, paste(start1B, ":1B:2B::", sep="")))
  advanced.two = with(ds1, str_count(runnerMovement, paste(start1B, ":1B:3B::", sep="")))
  basesAdvanced = ifelse(scored1B == 1, 3, ifelse(out1B == 1, -1, ifelse(advanced.one == 1, 1, ifelse(advanced.two == 1, 2, 0))))
  endCode.br1 = ifelse(basesAdvanced == 0, 1, ifelse(basesAdvanced == 1, 2, ifelse(basesAdvanced == 3, 4, 0)))
  endOuts.br1 = with(ds1, ifelse(basesAdvanced == -1, startOuts + 1, startOuts)) 
  ds1 = transform(ds1, delta.br1 = fit.rem(endCode.br1, endOuts.br1) - fit.rem(rep(1, nrow(ds1)), startOuts) + (basesAdvanced == 3))
  
  mod.br3 = lm(delta.br3 ~ event + startOuts, data = ds3)
  mod.br2 = lm(delta.br2 ~ event + startOuts, data = ds2)
  mod.br1 = lm(delta.br1 ~ event + startOuts, data = ds1)
  data[br3.idx, "delta.br3"] = ds3$delta.br3
  data[br2.idx, "delta.br2"] = ds2$delta.br2
  data[br1.idx, "delta.br1"] = ds1$delta.br1
  data[br3.idx, "raa.br3"] = mod.br3$residuals
  data[br2.idx, "raa.br2"] = mod.br2$residuals
  data[br1.idx, "raa.br1"] = mod.br1$residuals
  
  # Step 5: Define RAA for the batter
  message("...Estimating Batting Runs Above Average...")
  data = transform(data, delta.bat = delta - ifelse(is.na(delta.br1), 0, delta.br1) 
                   - ifelse(is.na(delta.br2), 0, delta.br2) - ifelse(is.na(delta.br3), 0, delta.br3))
  mod.bat = lm(delta.bat ~ batterPos + stadium + (stand == throws), data=data)
#  summary(mod.bat)
  data = transform(data, raa.bat = mod.bat$residuals)
  return(data)
}

#' @title getWAR
#' 
#' @description Retrieve openWAR
#' 
#' @details Retrieves openWAR, aggregated by player, given an MLBAM data set
#' 
#' @param data An MLBAM data.frame 
#' 
#' @return a data.frame of RAA values per player
#' 
#' @export
#' @examples
#' 
#' ds = getData()
#' res = getWAR(ds)
#' 

getWAR = function (data, ...) {
  ds = makeWAR(data)
  
  require(plyr)
  war.bat = ddply(ds, ~ batterId, summarise, Name = max(as.character(batterName))
                  , PA = length(batterId), G = length(unique(gameId)), HR = sum(event=="Home Run")
                  , RAA = sum(delta, na.rm=TRUE), RAA.bat = sum(raa.bat, na.rm=TRUE))
  war.br1 = ddply(ds, ~start1B, summarise, RAA.br1 = sum(raa.br1, na.rm=TRUE))
  war.br2 = ddply(ds, ~start2B, summarise, RAA.br2 = sum(raa.br2, na.rm=TRUE))
  war.br3 = ddply(ds, ~start3B, summarise, RAA.br3 = sum(raa.br3, na.rm=TRUE))
  
  war.P = ddply(ds, ~pitcherId, summarise, RAA.P = sum(raa.P, na.rm=TRUE))
  war.C = ddply(ds, ~playerId.C, summarise, RAA.C = sum(raa.C, na.rm=TRUE))
  war.1B = ddply(ds, ~playerId.1B, summarise, RAA.1B = sum(raa.1B, na.rm=TRUE))
  war.2B = ddply(ds, ~playerId.2B, summarise, RAA.2B = sum(raa.2B, na.rm=TRUE))
  war.3B = ddply(ds, ~playerId.3B, summarise, RAA.3B = sum(raa.3B, na.rm=TRUE))
  war.SS = ddply(ds, ~playerId.SS, summarise, RAA.SS = sum(raa.SS, na.rm=TRUE))
  war.LF = ddply(ds, ~playerId.LF, summarise, RAA.LF = sum(raa.LF, na.rm=TRUE))
  war.CF = ddply(ds, ~playerId.CF, summarise, RAA.CF = sum(raa.CF, na.rm=TRUE))
  war.RF = ddply(ds, ~playerId.RF, summarise, RAA.RF = sum(raa.RF, na.rm=TRUE))
  war.pitch = ddply(ds, ~ pitcherId, summarise, Name = max(as.character(pitcherName)), RAA.pitch = sum(raa.pitch))
  
  players = merge(x=war.bat, y=war.br1, by.x="batterId", by.y="start1B", all=TRUE)
  players = merge(x=players, y=war.br2, by.x="batterId", by.y="start2B", all=TRUE)
  players = merge(x=players, y=war.br3, by.x="batterId", by.y="start3B", all=TRUE)
  players[is.na(players)] = 0
  players = transform(players, RAA.br = RAA.br1 + RAA.br2 + RAA.br3)
  players = transform(players, RAA = RAA.bat + RAA.br)
  players = merge(x=players, y=war.pitch, by.x="batterId", by.y="pitcherId", all=TRUE)
  players$Name = with(players, ifelse(is.na(Name.x), Name.y, Name.x))
  players = merge(x=players, y=war.P, by.x="batterId", by.y="pitcherId", all=TRUE)
  players = merge(x=players, y=war.C, by.x="batterId", by.y="playerId.C", all=TRUE)
  players = merge(x=players, y=war.1B, by.x="batterId", by.y="playerId.1B", all=TRUE)
  players = merge(x=players, y=war.2B, by.x="batterId", by.y="playerId.2B", all=TRUE)
  players = merge(x=players, y=war.3B, by.x="batterId", by.y="playerId.3B", all=TRUE)
  players = merge(x=players, y=war.SS, by.x="batterId", by.y="playerId.SS", all=TRUE)
  players = merge(x=players, y=war.LF, by.x="batterId", by.y="playerId.LF", all=TRUE)
  players = merge(x=players, y=war.CF, by.x="batterId", by.y="playerId.CF", all=TRUE)
  players = merge(x=players, y=war.RF, by.x="batterId", by.y="playerId.RF", all=TRUE)
  players[is.na(players)] = 0
  players = transform(players, RAA.field = RAA.P + RAA.C + RAA.1B + RAA.2B + RAA.3B + RAA.SS + RAA.LF + RAA.CF + RAA.RF)
  players = transform(players, RAA = RAA.bat + RAA.br + RAA.pitch + RAA.field)
  return(players)
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
  
  mod.P = glm((fielderPos == "P") ~ poly(our.x, 2) + poly(our.y, 2), data=ds, family="binomial")
  mod.C = glm((fielderPos == "C") ~ poly(our.x, 2) + poly(our.y, 2), data=ds, family="binomial")
  mod.1B = glm((fielderPos == "1B") ~ poly(our.x, 2) + poly(our.y, 2), data=ds, family="binomial")
  mod.2B = glm((fielderPos == "2B") ~ poly(our.x, 2) + poly(our.y, 2), data=ds, family="binomial")
  mod.3B = glm((fielderPos == "3B") ~ poly(our.x, 2) + poly(our.y, 2), data=ds, family="binomial")
  mod.SS = glm((fielderPos == "SS") ~ poly(our.x, 2) + poly(our.y, 2), data=ds, family="binomial")
  mod.LF = glm((fielderPos == "LF") ~ poly(our.x, 2) + poly(our.y, 2), data=ds, family="binomial")
  mod.CF = glm((fielderPos == "CF") ~ poly(our.x, 2) + poly(our.y, 2), data=ds, family="binomial")
  mod.RF = glm((fielderPos == "RF") ~ poly(our.x, 2) + poly(our.y, 2), data=ds, family="binomial")
  out = data.frame(mod.P$fitted, mod.C$fitted, mod.1B$fitted, mod.2B$fitted, mod.3B$fitted
              , mod.SS$fitted, mod.LF$fitted, mod.CF$fitted, mod.RF$fitted)
  row.sums = apply(out, 1, sum)
  out = out / row.sums
  names(out) = c("resp.P", "resp.C", "resp.1B", "resp.2B", "resp.3B", "resp.SS", "resp.LF", "resp.CF", "resp.RF")
  return(out)
}

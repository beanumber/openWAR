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
  ds.field = subset(data, isBIP == TRUE)
  outs = subset(ds.field, !is.na(fielderId), select=c("our.x", "our.y"))
  hits = subset(ds.field, is.na(fielderId), select=c("our.x", "our.y"))
  # Find 2D kernel density estimates for hits and outs
  # Make sure to specify the range, so that they over estimated over the same grid
  grid = list(range(ds$our.x, na.rm=TRUE), range(ds$our.y, na.rm=TRUE))
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

  resp.field = mapply(fit.all, ds.field$our.x, ds.field$our.y)
  
  # Stitch the new data back onto the old
  field.idx = which(data$isBIP == TRUE)
  out = data$our.x
  out[field.idx] = resp.field
  return(out)
}

#' @title setBIPresp
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

setBIPresp = function (data, ...) {
  require(mosaic)
  ds = transform(ds, wasFielded = !is.na(fielderId))
  data$fielderPos = ifelse(is.na(data$fielderId), "Hit", "Out")
  data$fielderPos = ifelse(!is.na(data$fielderId) & data$fielderId == data$pitcherId, "P", data$fielderPos)
  data$fielderPos = ifelse(!is.na(data$fielderId) & data$fielderId == data$playerId.C, "C", data$fielderPos)
  data$fielderPos = ifelse(!is.na(data$fielderId) & data$fielderId == data$playerId.1B, "1B", data$fielderPos)
  data$fielderPos = ifelse(!is.na(data$fielderId) & data$fielderId == data$playerId.2B, "2B", data$fielderPos)
  data$fielderPos = ifelse(!is.na(data$fielderId) & data$fielderId == data$playerId.3B, "3B", data$fielderPos)
  data$fielderPos = ifelse(!is.na(data$fielderId) & data$fielderId == data$playerId.SS, "SS", data$fielderPos)
  data$fielderPos = ifelse(!is.na(data$fielderId) & data$fielderId == data$playerId.LF, "LF", data$fielderPos)
  data$fielderPos = ifelse(!is.na(data$fielderId) & data$fielderId == data$playerId.CF, "CF", data$fielderPos)
  data$fielderPos = ifelse(!is.na(data$fielderId) & data$fielderId == data$playerId.RF, "RF", data$fielderPos)
  
  # To build the fielding models, use only the subset of the data in which the ball was in play
  ds.field = subset(data, isBIP == TRUE)
  mod.P = glm((fielderPos == "P") ~ poly(our.x, 2) + poly(our.y, 2), data=ds.field, family="binomial")
  mod.C = glm((fielderPos == "C") ~ poly(our.x, 2) + poly(our.y, 2), data=ds.field, family="binomial")
  mod.1B = glm((fielderPos == "1B") ~ poly(our.x, 2) + poly(our.y, 2), data=ds.field, family="binomial")
  mod.2B = glm((fielderPos == "2B") ~ poly(our.x, 2) + poly(our.y, 2), data=ds.field, family="binomial")
  mod.3B = glm((fielderPos == "3B") ~ poly(our.x, 2) + poly(our.y, 2), data=ds.field, family="binomial")
  mod.SS = glm((fielderPos == "SS") ~ poly(our.x, 2) + poly(our.y, 2), data=ds.field, family="binomial")
  mod.LF = glm((fielderPos == "LF") ~ poly(our.x, 2) + poly(our.y, 2), data=ds.field, family="binomial")
  mod.CF = glm((fielderPos == "CF") ~ poly(our.x, 2) + poly(our.y, 2), data=ds.field, family="binomial")
  mod.RF = glm((fielderPos == "RF") ~ poly(our.x, 2) + poly(our.y, 2), data=ds.field, family="binomial")
  out = data.frame(mod.P$fitted, mod.C$fitted, mod.1B$fitted, mod.2B$fitted, mod.3B$fitted
              , mod.SS$fitted, mod.LF$fitted, mod.CF$fitted, mod.RF$fitted)
  row.sums = apply(out, 1, sum)
  out = out / row.sums
  
  # Stitch the new data back onto the old
  field.idx = which(data$isBIP == TRUE)
  data[field.idx, "resp.P"] = out$mod.P.fitted
  data[field.idx, "resp.C"] = out$mod.C.fitted
  data[field.idx, "resp.1B"] = out$mod.1B.fitted
  data[field.idx, "resp.2B"] = out$mod.2B.fitted
  data[field.idx, "resp.3B"] = out$mod.3B.fitted
  data[field.idx, "resp.SS"] = out$mod.SS.fitted
  data[field.idx, "resp.LF"] = out$mod.LF.fitted
  data[field.idx, "resp.CF"] = out$mod.CF.fitted
  data[field.idx, "resp.RF"] = out$mod.RF.fitted
  return(data)
}

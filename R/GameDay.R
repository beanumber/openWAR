#' @title gameday
#' @aliases gameday.default
#' 
#' @description Convenience class for dealing with MLBAM GameDay files 
#' 
#' @details Each gameId corresponds to one object of class "gameday"
#' 
#' @param gameId A valid MLBAM gameId
#' 
#' @return An object of class "gameday", which consists of a list containing
#' \item{gameId}{The ID of the game (e.g. "gid_2012_08_12_atlmlb_nynmlb_1")}
#' \item{base}{The base URL for the game (e.g. "http://gd2.mlb.com/components/game/mlb/year_2012/month_08/day_12/")}
#' \item{url}{A character vector containing the names of the XML files downloaded from the GameDay server}
#' \item{ds}{The processed data set for use with openWAR containing all play-by-play information for the game indicated by the gameId}
#' 
#' @export
#' @examples
#' 
#' exampleGameday <- gameday(gameId = "gid_2012_08_12_atlmlb_nynmlb_1")
#' #Display GameId
#' exampleGameday$gameId
#' #Summary of the different types fo game events for gameId = "gid_2012_08_12_atlmlb_nynmlb_1"
#' summary(exampleGameday$ds$event)



gameday = function (gameId = "gid_2012_08_12_atlmlb_nynmlb_1", ...) {
  # Check to make the gameId is at least potentially valid
  if (nchar(gameId) != 30) {
    stop("This is not a valid MLBAM gameId!")
  }
  yyyy <- substring(gameId, 5, 8)
  mm <- substring(gameId, 10, 11)
  dd <- substring(gameId, 13, 14)
  
  # Base URL
  base = paste("http://gd2.mlb.com/components/game/mlb/year_",yyyy,"/month_",mm,"/day_",dd,"/",sep="")
  message(gameId)
  
  gd = list("gameId" = gameId, "base" = base)
  class(gd) = "gameday"
  gd$url = getURLs.gameday(gd)
#  gd$xml = getXML.gameday(gd)
#  if (!is.null(gd$xml)) {
#    gd$ds = getData.gameday(gd)
#  }
  try(gd$ds <- readData.gameday(gd))
  return(gd)
}


###############################################################################################
#
# Utility functions
#
###############################################################################################

#' @title flush.gameday
#' 
#' @description Remove a troublesome gameday cache
#' 
#' @details Given a gameday object or gameId, delete all local cache files
#' 
#' @param gd A gameday object
#' @param gameId A valid MLBAM gameId
#' 
#' @return NULL
#' 
#' @examples
#' 
#' gd = gameday()
#' getURLs.gameday(gd)


flush.gameday = function (x) {
  if (class(x) == "gameday") {
    gameId = gd$gameId
  } else {
    gameId = x
  }
  dirname = file.path(find.package("openWAR"), "data", gameId)
  if (file.exists(dirname)) {
    unlink(dirname, recursive=TRUE)
  }
}


#' @title getURLs.gameday
#' @aliases getXML.gameday
#' 
#' @description Convenience function for returning data related to a game
#' 
#' @details Given a gameId string, this function can return several URLs
#' 
#' @param gameId A valid MLBAM gameId
#' @param type The type of file 
#' 
#' @return a URL, or an XML file
#' 
#' @examples
#' 
#' gd = gameday()
#' getURLs.gameday(gd)


getURLs.gameday = function (gd) {
  url = NULL
  url["bis_boxscore.xml"] = paste(gd$base, gd$gameId, "/bis_boxscore.xml", sep="")  
  url["inning_all.xml"] = paste(gd$base, gd$gameId, "/inning/inning_all.xml", sep="")
  url["inning_hit.xml"] = paste(gd$base, gd$gameId, "/inning/inning_hit.xml", sep="")  
  url["game.xml"] = paste(gd$base, gd$gameId, "/game.xml", sep="")  
  url["game_events.xml"] = paste(gd$base, gd$gameId, "/game_events.xml", sep="")    
  return(url)
}

getXML.gameday = function (gd) {
  require(XML)
  require(RCurl)
  
  # If the local directory for this game does not exist, create it and download the files
  dirname = file.path(find.package("openWAR"), "data", gd$gameId)
  if (!file.exists(dirname)) {
    warning("...GameDay XML files are not in local directory -- must download")
    dir.create(dirname, recursive=TRUE)  
    curl = getCurlHandle()
    files = getURL(gd$url, curl = curl)
    for(i in 1:(length(files))) {
      filename = basename(names(files)[i])
      write(files[i], file.path(dirname, filename))
    }
  }
  
  # Now read from the local files
  local.filenames = file.path(dirname, names(gd$url))
  
  xml = try(sapply(local.filenames, xmlParse))
  if (class(xml) == "try-error") {
    warning("404 - GameDay files do not exist...")
    return(NULL)
  } else {
    names(xml) = names(gd$url)
  
#   xml = switch(type
#                , bis_boxscore.xml = try(xmlParse(file.path(dirname, "bis_boxscore.xml")))
#                , inning_all.xml = try(xmlTreeParse(file.path(dirname, "inning_all.xml")))
#                , game.xml = try(xmlParse(file.path(dirname, "game.xml")))
#                , game_events.xml = try(xmlTreeParse(file.path(dirname, "game_events.xml")))
#                , inning_hit.xml = try(xmlTreeParse(file.path(dirname, "inning_hit.xml")))
#   )
#    closeAllConnections()
    return(xml)
  }
}

#' @title readData.gameday
#' 
#' @description Grabs XML files from MLBAM and converts to data.frames in memory
#' 
#' @details Grabs XML files from MLBAM and converts to data.frames in memory
#' 
#' @param gd A gameday object
#' 
#' @return A gameday object
#' 
#' @export
#' @examples
#' 
#' gd = gameday()
#' getURLs.gameday(gd)


readData.gameday = function (gd) {
  require(mosaic)
  # First, build gd$data as a list of data frames
  dirname = file.path(find.package("openWAR"))
  labels = do.call("rbind", strsplit(basename(gd$url), split="\\."))[,1]
  for (i in 1:length(labels)) { 
    xsl = paste(dirname, "/xsl/", labels[i], ".xsl", sep="")
    # Use the shell command 'xsltproc'
#    cmd = paste("xsltproc", xsl, gd$url[i], sep=" ")
#    dat = try(system(cmd, intern=TRUE))
    # Alternative within R
    require(Sxslt)
    # apply the stylesheet to the XML
    dat = str_split(saveXML(xsltApplyStyleSheet(gd$url[i], xsl)$doc), "\n")[[1]]
    # remove the xml description on the first line
    dat = dat[-1]
    # remove any blank lines
    dat = dat[dat != ""]
    
    if (!is.null(attr(dat, "status"))) { break; }
    df = as.data.frame(do.call("rbind", strsplit(dat[-1], split="\\|")))
    if (nrow(df) == 0) { break; }
    names(df) = strsplit(dat[1], split="\\|")[[1]]
    gd$data[[labels[[i]][1]]] = df
  }
  
  if (length(gd$data) == 5) {
    # If any timestamps are missing, set them equal to the previous timstamp
    missing.idx = which(gd$data$game_events$timestamp == "")
    gd$data$game_events$timestamp[missing.idx] = gd$data$game_events$timestamp[missing.idx - 1]
    
    # Now, merge the data frames into one large data frame
    out = merge(x=gd$data$inning_all, y=gd$data$inning_hit[,c("inning", "batterId", "pitcherId", "event", "x", "y")], by=c("inning", "batterId", "pitcherId", "event"), all.x=TRUE)
    out = merge(x=gd$data$game_events, y=out, by = "ab_num", all.x=TRUE)
    out = cbind(out, gd$data$game)
    
    # Make sure R understands that timestamp is a date
    out$timestamp = as.character(strptime(out$timestamp.x, format="%Y-%m-%dT%H:%M:%S"))
    
    # Merge again to get the starting fielders
    out$field_teamId = ifelse(out$half == "top", as.character(out$home_teamId), as.character(out$away_teamId))
    # Create a lookup for the player's name
    lkup = gd$data$bis_boxscore
    lkup = lkup[unique(lkup$playerId),]
    lkup$bo = as.numeric(as.character(lkup$bo))
    # Weed out pitchers
    lineup = subset(lkup, bo %% 100 == 0)
    lineup$batterPos = sapply(strsplit(as.character(lineup$pos), split="-"), "[", 1)
    lineup.long = lineup[order(lineup$teamId, lineup$batterPos),]
    lineup.wide = reshape(lineup.long, v.names = "playerId", timevar = "batterPos", idvar = "teamId", direction="wide", drop=c("playerName", "bo", "pos"))
    def.pos = c("playerId.C", "playerId.1B", "playerId.2B", "playerId.3B", "playerId.SS", "playerId.LF", "playerId.CF", "playerId.RF")
    out = merge(x=out, y=lineup.wide[,c("teamId", def.pos)], by.x = "field_teamId", by.y = "teamId", all.x=TRUE)
    
    # Grab the defensive position of the batter
    out = merge(x=out, y=lineup[,c("playerId", "batterPos")], by.x = "batterId", by.y = "playerId", all.x=TRUE)
    
    # Grab the batters and pitchers names
    out = merge(x=out, y=lkup[,c("playerId", "playerName")], by.x = "batterId", by.y = "playerId", all.x=TRUE)
    out = merge(x=out, y=lkup[,c("playerId", "playerName")], by.x = "pitcherId", by.y = "playerId", all.x=TRUE)
    
    # Clean up some column names
    idx = which(names(out) %in% c("inning.x", "event.x", "batterPos", "playerName.x", "playerName.y"))
    names(out)[idx] = c("inning", "event", "batterPos", "batterName", "pitcherName")
    out = out[,!names(out) %in% c("inning.y", "event.y", "timestamp.x", "timestamp.y")]
    
    # Convert columns to numerics
    cols = c("ab_num", "balls", "strikes", "endOuts", "batterId", "pitcherId"
             , "actionId", "home_teamId", "away_teamId", "venueId"
             , "inning", "x", "y", "playerId.1B", "playerId.2B", "playerId.3B", "playerId.C", "playerId.CF"
             , "playerId.LF", "playerId.P", "playerId.RF", "playerId.SS")
    colIds = which(names(out) %in% cols)
    for (colId in colIds) {
      #      cat(colId)
      out[,colId] = as.numeric(as.character(out[,colId]))
    }
    
    # Update fielders for defensive switches
    require(stringr)
#    require(plyr)
    out = makeSubstitutions(out)  
    # Sometimes data errors result in the batter's defensive position not getting set. Set it to UN manually
    missing.idx = which(is.na(out$batterPos) & out$batterId > 0)
    out$batterPos[missing.idx] = "UN"
    
    # Experimental -- I think we can filter out non-inning-all events now
    out = subset(out, !is.na(batterId))
    out = subset(out, !event %in% c("Game Advisory", "Defensive Switch", "Offensive sub", "Pitching Substitution"
                                    , "Defensive Sub", "Player Injured", "Ejection", "Umpire Substitution"))
    
    # Runs on play
    #    out$runsOnPlay = str_count(out$description, " scores.") + str_count(out$description, "homers")
    out$runsOnPlay = str_count(as.character(out$runnerMovement), ":T:")
    
    # runner movement, score, and outs
#    out = ddply(out, ~inning + half, updateHalfInning)  
    out <- dplyr::do(group_by(out, inning, half), updateHalfInning(.))
    # Once the non-PA related events have been removed, best to sort by ab_num, since the timestamp is occassionally missing!
    out = out[order(out$ab_num),]
    
    # number of baserunners
    out$startCode = as.numeric((1 * !is.na(as.data.frame(out)[, c("start1B", "start2B", "start3B")])) %*% c(1, 2, 4))
    out$endCode  = as.numeric((1 * !is.na(as.data.frame(out)[, c("end1B", "end2B", "end3B")])) %*% c(1, 2, 4))
    
    # Figure out who fielded the ball
    out$fielderId = getFielderId(out)
      
    # double-check -- should work for all normal plays, but fails on third out
  #  out$startBR = (1 * !is.na(out[, c("start1B", "start2B", "start3B")])) %*% c(1, 1, 1)
  #  out$endBR = (1 * !is.na(out[, c("end1B", "end2B", "end3B")])) %*% c(1, 1, 1)
  #  with(out, startBR + 1 - endBR == runsOnPlay + endOuts - startOuts)
    
    out$gameId = as.character(gd$gameId)
    out = out[order(out$ab_num),]
    
    # add some convenience calculation fields
    out = mutate(out, isPA = !event %in% c("Defensive Indiff", "Stolen Base 2B", "Runner Out"))
    out = mutate(out, isAB = isPA & !event %in% c("Walk", "Intent Walk", "Hit By Pitch", "Sac Fly", "Sac Bunt"))
    out = mutate(out, isHit = event %in% c("Single", "Double", "Triple", "Home Run"))
    out = mutate(out, isBIP = event != "Home Run" & !is.na(x) & !is.na(y))
    
    # translate the coordinates so that home plate is (0,0) and second base is (0, 127' 3 3/8")
    out = recenter(out)
  } else {
    warning("Game data is no bueno -- most likely a rainout")
    out = NULL
  }
  return(out)
}

# data = subset(out, inning == 9 & half == "bottom")
# fixRunnerMovement(data)

updateHalfInning = function (data) {
#  cat(print("new inning"))
  # IMPORTANT: Have to sort the data frame just in case
#  data = data[order(data$timestamp),]
  # Sometimes the timestamp is blank -- so sort these by AB num
  data = data[order(data$ab_num),]
  data$startOuts = 0 
  data$runsInInning = sum(data$runsOnPlay)
  data$runsITD = cumsum(c(0, data$runsOnPlay))[1:nrow(data)]
  data = transform(data, runsFuture = runsInInning - runsITD)
  data$start1B = as.character(NA)
  data$start2B = as.character(NA)
  data$start3B = as.character(NA)
  data$end1B = as.character(NA)
  data$end2B = as.character(NA)
  data$end3B = as.character(NA)
  leadoff.mv = getRunnerMovement(data[1, "runnerMovement"])
  data[1, "end1B"] = leadoff.mv["end1B"]
  data[1, "end2B"] = leadoff.mv["end2B"]
  data[1, "end3B"] = leadoff.mv["end3B"]
  if (nrow(data) > 1) {
    for (i in 2:nrow(data)) {
#      cat(print(i))
      data[i, "startOuts"] = data[i-1, "endOuts"]
      data[i, "start1B"] = data[i-1, "end1B"]
      data[i, "start2B"] = data[i-1, "end2B"]
      data[i, "start3B"] = data[i-1, "end3B"]
      runner.mv = getRunnerMovement(data[i, "runnerMovement"])
      if (!is.na(runner.mv["end1B"])) { 
        data[i, "end1B"] = runner.mv["end1B"]
      } else {
        if (!is.na(data[i, "start1B"]) & is.na(runner.mv["start1B"])) {
          data[i, "end1B"] = data[i, "start1B"]
        }
      }
      if (!is.na(runner.mv["end2B"])) { 
        data[i, "end2B"] = runner.mv["end2B"]
      } else {
        if (!is.na(data[i, "start2B"]) & is.na(runner.mv["start2B"])) {
          data[i, "end2B"] = data[i, "start2B"]
        }
      }
      if (!is.na(runner.mv["end3B"])) { 
        data[i, "end3B"] = runner.mv["end3B"]
      } else {
        if (!is.na(data[i, "start3B"]) & is.na(runner.mv["start3B"])) {
          data[i, "end3B"] = data[i, "start3B"]
        }
      }
      # Always end the inning with nobody on
  #    if (data[i, "endOuts"] == 3) { 
  #      data[i, "end1B"] = NA
  #      data[i, "end2B"] = NA
  #      data[i, "end3B"] = NA
  #    }
    }
  }
  # Force the bases empty at the end of every inning
  data[nrow(data), "end1B"] = NA
  data[nrow(data), "end2B"] = NA
  data[nrow(data), "end3B"] = NA
  data$outsInInning = sum(data$endOuts - data$startOuts)
  return(data)
}


getRunnerMovement = function (x) {
  # runner movement
  y = as.character(x)
  rm.vec = rep(NA, 6)
  names(rm.vec) = c("start1B", "start2B", "start3B", "end1B", "end2B", "end3B")
  if (!is.na(x) & nchar(y[1]) > 0) {
    z = substr(y, start=2, stop=(nchar(y) - 1))
    rm.df = as.data.frame(do.call("rbind", str_split(unlist(str_split(z, "\\]\\[")), ":")), stringsAsFactors = FALSE)
    names(rm.df) = c("id", "start", "end", "event")
    
    # In case a runner moved twice during the at-bat, for now just concentrate on where he ended up
    rm.df$end = ifelse(rm.df$end == "", "4B", rm.df$end)
    # rm.df = ddply(rm.df, ~ id, summarise, start = min(start), end = max(end))
    rm.df <- summarise(group_by(rm.df, id), start = min(start), end = max(end))
    rm.df$end = ifelse(rm.df$end == "4B", "", rm.df$end)
    if( nrow(subset(rm.df, start == "1B")) > 0 ) { rm.vec["start1B"] = subset(rm.df, start == "1B")$id }
    if( nrow(subset(rm.df, start == "2B")) > 0 ) { rm.vec["start2B"] = subset(rm.df, start == "2B")$id }
    if( nrow(subset(rm.df, start == "3B")) > 0 ) { rm.vec["start3B"] = subset(rm.df, start == "3B")$id }
    if( nrow(subset(rm.df, end == "1B")) > 0 ) { rm.vec["end1B"] = subset(rm.df, end == "1B")$id }
    if( nrow(subset(rm.df, end == "2B")) > 0 ) { rm.vec["end2B"] = subset(rm.df, end == "2B")$id }
    if( nrow(subset(rm.df, end == "3B")) > 0 ) { rm.vec["end3B"] = subset(rm.df, end == "3B")$id }
    #  rm.vec = as.numeric(rm.vec)
  }
  return(rm.vec)
}



makeSubstitutions = function (data) {
  require(stringr)
  # IMPORTANT: Have to sort the data frame just in case
  # Have to sort by timestamp here, NOT by ab_num!
  data = data[order(data$timestamp),]
  n = nrow(data)
  top = which(data$half == "top")
  bottom = which(data$half != "top")
  
  # Pinch-hitters
  idx = which(data$event %in% c("Offensive sub"))
  for(i in idx) {
    if (str_count(data[i,"description"], "Pinch-hitter")) { 
      x = which(data$batterId == data[i, "actionId"])
      # You can only pinch-hit once, and it must be the first time you appeared in the game
      if (length(x) > 0) {
        data[x[1], "batterPos"] = "PH"
      }
    }
  }
  
  # Defensive substitutions
  idx = which(data$event %in% c("Defensive Switch", "Defensive Sub", "Pitching Substitution"))
  for(i in idx) {
    if (data[i, "half"] == "top") {
      off = bottom
      def = top
    } else {
      off = top
      def = bottom
    }
    if (str_count(data[i,"description"], "Pitching Change:")) { 
      data[intersect(i:n, which(data$batterId == data[i,"actionId"])), "batterPos"] = "P"
    }
    if (str_count(data[i,"description"], "(to designated hitter|as the designated hitter|playing designated hitter)")) { 
      data[intersect(i:n, which(data$batterId == data[i,"actionId"])), "batterPos"] = "DH"
    }
    if (str_count(data[i,"description"], "(to catcher|as the catcher|playing catcher)")) { 
      data[intersect(i:n, def), "playerId.C"] = data[i,"actionId"]
      data[intersect(i:n, which(data$batterId == data[i,"actionId"])), "batterPos"] = "C"
    }
    if (str_count(data[i,"description"], "(to first base|as the first baseman|playing first base)")) { 
      data[intersect(i:n, def), "playerId.1B"] = data[i,"actionId"]
      data[intersect(i:n, which(data$batterId == data[i,"actionId"])), "batterPos"] = "1B"
    }
    if (str_count(data[i,"description"], "(to second base|as the second baseman|playing second base)")) { 
      data[intersect(i:n, def), "playerId.2B"] = data[i,"actionId"]
      data[intersect(i:n, which(data$batterId == data[i,"actionId"])), "batterPos"] = "2B"
    }
    if (str_count(data[i,"description"], "(to third base|as the third baseman|playing third base)")) { 
      data[intersect(i:n, def), "playerId.3B"] = data[i,"actionId"]
      data[intersect(i:n, which(data$batterId == data[i,"actionId"])), "batterPos"] = "3B"
    }
    if (str_count(data[i,"description"], "(to shortstop|as the shortstop|playing shortstop)")) { 
      data[intersect(i:n, def), "playerId.SS"] = data[i,"actionId"]
      data[intersect(i:n, which(data$batterId == data[i,"actionId"])), "batterPos"] = "SS"
    }
    if (str_count(data[i,"description"], "(to left field|as the left fielder|playing left field)")) { 
      data[intersect(i:n, def), "playerId.LF"] = data[i,"actionId"]
      data[intersect(i:n, which(data$batterId == data[i,"actionId"])), "batterPos"] = "LF"
    }
    if (str_count(data[i,"description"], "(to center field|as the center fielder|playing center field)")) { 
      data[intersect(i:n, def), "playerId.CF"] = data[i,"actionId"]
      data[intersect(i:n, which(data$batterId == data[i,"actionId"])), "batterPos"] = "CF"
    }
    if (str_count(data[i,"description"], "(to right field|as the right fielder|playing right field)")) { 
      data[intersect(i:n, def), "playerId.RF"] = data[i,"actionId"]
      data[intersect(i:n, which(data$batterId == data[i,"actionId"])), "batterPos"] = "RF"
    }
  }
  # double-check to make sure you always have 9 distinct defenders
#  X = data[!is.na(data$batterId), c("playerId.C", "playerId.1B", "playerId.2B", "playerId.3B", "playerId.SS", "playerId.LF", "playerId.CF", "playerId.RF")]
#  sum(apply(X, 1, duplicated))

  return(data)
}

getFielderId = function (data) {
  
  # Figure out who fielded the ball
  # Flyouts
  fielderId = with(data, ifelse(event %in% c("Flyout", "Lineout", "Pop Out", "Bunt Pop Out") & str_count(description, " (flies|lines|pops) out( sharply| softly)? to pitcher "), pitcherId, NA))
  fielderId = with(data, ifelse(event %in% c("Flyout", "Lineout", "Pop Out", "Bunt Pop Out") & str_count(description, " (flies|lines|pops) out( sharply| softly)? to catcher "), playerId.C, fielderId))
  fielderId = with(data, ifelse(event %in% c("Flyout", "Lineout", "Pop Out", "Bunt Pop Out") & str_count(description, " (flies|lines|pops) out( sharply| softly)? to first baseman "), playerId.1B, fielderId))
  fielderId = with(data, ifelse(event %in% c("Flyout", "Lineout", "Pop Out", "Bunt Pop Out") & str_count(description, " (flies|lines|pops) out( sharply| softly)? to second baseman "), playerId.2B, fielderId))
  fielderId = with(data, ifelse(event %in% c("Flyout", "Lineout", "Pop Out", "Bunt Pop Out") & str_count(description, " (flies|lines|pops) out( sharply| softly)? to third baseman "), playerId.3B, fielderId))
  fielderId = with(data, ifelse(event %in% c("Flyout", "Lineout", "Pop Out", "Bunt Pop Out") & str_count(description, " (flies|lines|pops) out( sharply| softly)? to shortstop "), playerId.SS, fielderId))
  fielderId = with(data, ifelse(event %in% c("Flyout", "Lineout", "Pop Out", "Bunt Pop Out") & str_count(description, " (flies|lines|pops) out( sharply| softly)? to left fielder "), playerId.LF, fielderId))
  fielderId = with(data, ifelse(event %in% c("Flyout", "Lineout", "Pop Out", "Bunt Pop Out") & str_count(description, " (flies|lines|pops) out( sharply| softly)? to center fielder "), playerId.CF, fielderId))
  fielderId = with(data, ifelse(event %in% c("Flyout", "Lineout", "Pop Out", "Bunt Pop Out") & str_count(description, " (flies|lines|pops) out( sharply| softly)? to right fielder "), playerId.RF, fielderId))
  # Groundouts with an assist
  fielderId = with(data, ifelse(event %in% c("Groundout", "Bunt Groundout") & str_count(description, " grounds out( sharply| softly)?(,| to) third baseman "), playerId.3B, fielderId))
  fielderId = with(data, ifelse(event %in% c("Groundout", "Bunt Groundout") & str_count(description, " grounds out( sharply| softly)?(,| to) shortstop "), playerId.SS, fielderId))
  fielderId = with(data, ifelse(event %in% c("Groundout", "Bunt Groundout") & str_count(description, " grounds out( sharply| softly)?(,| to) second baseman "), playerId.2B, fielderId))
  fielderId = with(data, ifelse(event %in% c("Groundout", "Bunt Groundout") & str_count(description, " grounds out( sharply| softly)?(,| to) first baseman "), playerId.1B, fielderId))
  fielderId = with(data, ifelse(event %in% c("Groundout", "Bunt Groundout") & str_count(description, " grounds out( sharply| softly)?(,| to) pitcher "), pitcherId, fielderId))
  fielderId = with(data, ifelse(event %in% c("Groundout", "Bunt Groundout") & str_count(description, " grounds out( sharply| softly)?(,| to) catcher "), playerId.C, fielderId))
  # Forceout
  fielderId = with(data, ifelse(event == "Forceout" & str_count(description, " (grounds|pops)( sharply| softly)? into a force out,( fielded by)? third baseman "), playerId.3B, fielderId))
  fielderId = with(data, ifelse(event == "Forceout" & str_count(description, " (grounds|pops)( sharply| softly)? into a force out,( fielded by)? shortstop "), playerId.SS, fielderId))
  fielderId = with(data, ifelse(event == "Forceout" & str_count(description, " (grounds|pops)( sharply| softly)? into a force out,( fielded by)? second baseman "), playerId.2B, fielderId))
  fielderId = with(data, ifelse(event == "Forceout" & str_count(description, " (grounds|pops)( sharply| softly)? into a force out,( fielded by)? first baseman "), playerId.1B, fielderId))
  fielderId = with(data, ifelse(event == "Forceout" & str_count(description, " (grounds|pops)( sharply| softly)? into a force out,( fielded by)? pitcher "), pitcherId, fielderId))
  fielderId = with(data, ifelse(event == "Forceout" & str_count(description, " (grounds|pops)( sharply| softly)? into a force out,( fielded by)? catcher "), playerId.C, fielderId))
  # Grounded Into DP
  fielderId = with(data, ifelse(event == "Grounded Into DP" & str_count(description, " grounds( sharply| softly)? into a double play, catcher "), playerId.C, fielderId))
  fielderId = with(data, ifelse(event == "Grounded Into DP" & str_count(description, " grounds( sharply| softly)? into a double play, third baseman "), playerId.3B, fielderId))
  fielderId = with(data, ifelse(event == "Grounded Into DP" & str_count(description, " grounds( sharply| softly)? into a double play, shortstop "), playerId.SS, fielderId))
  fielderId = with(data, ifelse(event == "Grounded Into DP" & str_count(description, " grounds( sharply| softly)? into a double play, second baseman "), playerId.2B, fielderId))
  fielderId = with(data, ifelse(event == "Grounded Into DP" & str_count(description, " grounds( sharply| softly)? into a double play, first baseman "), playerId.1B, fielderId))
  fielderId = with(data, ifelse(event == "Grounded Into DP" & str_count(description, " grounds( sharply| softly)? into a double play, pitcher "), pitcherId, fielderId))
  # Sac Bunts
  fielderId = with(data, ifelse(event %in% c("Sac Bunt", "Sac Fly", "Fielders Choice Out") & str_count(description, " on a (fielders choice out|sacrifice bunt|sacrifice fly)(,| to) pitcher "), pitcherId, fielderId))
  fielderId = with(data, ifelse(event %in% c("Sac Bunt", "Sac Fly", "Fielders Choice Out") & str_count(description, " on a (fielders choice out|sacrifice bunt|sacrifice fly)(,| to) catcher "), playerId.C, fielderId))
  fielderId = with(data, ifelse(event %in% c("Sac Bunt", "Sac Fly", "Fielders Choice Out") & str_count(description, " on a (fielders choice out|sacrifice bunt|sacrifice fly)(,| to) first baseman "), playerId.1B, fielderId))
  fielderId = with(data, ifelse(event %in% c("Sac Bunt", "Sac Fly", "Fielders Choice Out") & str_count(description, " on a (fielders choice out|sacrifice bunt|sacrifice fly)(,| to) second baseman "), playerId.2B, fielderId))
  fielderId = with(data, ifelse(event %in% c("Sac Bunt", "Sac Fly", "Fielders Choice Out") & str_count(description, " on a (fielders choice out|sacrifice bunt|sacrifice fly)(,| to) third baseman "), playerId.3B, fielderId))
  fielderId = with(data, ifelse(event %in% c("Sac Bunt", "Sac Fly", "Fielders Choice Out") & str_count(description, " on a (fielders choice out|sacrifice bunt|sacrifice fly)(,| to) shortstop "), playerId.SS, fielderId))
  fielderId = with(data, ifelse(event %in% c("Sac Bunt", "Sac Fly", "Fielders Choice Out") & str_count(description, " on a (fielders choice out|sacrifice bunt|sacrifice fly)(,| to) left fielder "), playerId.LF, fielderId))
  fielderId = with(data, ifelse(event %in% c("Sac Bunt", "Sac Fly", "Fielders Choice Out") & str_count(description, " on a (fielders choice out|sacrifice bunt|sacrifice fly)(,| to) center fielder "), playerId.CF, fielderId))
  fielderId = with(data, ifelse(event %in% c("Sac Bunt", "Sac Fly", "Fielders Choice Out") & str_count(description, " on a (fielders choice out|sacrifice bunt|sacrifice fly)(,| to) right fielder "), playerId.RF, fielderId))    
  
  return(fielderId)
}

recenter = function (data, ...) {
  # From MLBAM specs
  data = transform(data, our.x = x - 125)
  data = transform(data, our.y = 199 - y)
  # set distance from home to 2B
  scale = sqrt(90^2 + 90^2) / 51
  data = transform(data, r = scale * sqrt(our.x^2 + our.y^2))
  data = transform(data, theta = atan2(our.y, our.x))
  data = transform(data, our.x = r * cos(theta))
  data = transform(data, our.y = r * sin(theta))
  return(data)
}

#' @title gameday
#' @aliases gameday.default
#' 
#' @description Convenience class for dealing with MLBAM GameDay files 
#' 
#' @details Each gameId corresponds to one GameDay object
#' 
#' @param gameId A valid MLBAM gameId
#' 
#' @return a GameDay object, which consists of a list containing
#' \item{gameId}{The ID of the game}
#' \item{base}{the base URL for the game}
#' \item{xml}{a list of XML files downloaded from the GameDay server}
#' \item{ds}{the processed data set for use with openWAR}
#' 
#' @export
#' @examples
#' 
#' ds = getData()
#' getData(start = "2013-05-21", end = Sys.Date())

# gameday = function (gameId = "gid_2012_08_12_atlmlb_nynmlb_1", ...) UseMethod("gameday")

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
  # First, build gd$data as a list of data frames
  dirname = file.path(find.package("openWAR"))
  labels = do.call("rbind", strsplit(basename(gd$url), split="\\."))[,1]
  for (i in 1:length(labels)) { 
    cmd = paste("xsltproc ", dirname, "/xsl/", labels[i], ".xsl ", gd$url[i], sep="")
    dat = try(system(cmd, intern=TRUE))
    if (!is.null(attr(dat, "status"))) { break; }
    df = as.data.frame(do.call("rbind", strsplit(dat[-1], split="\\|")))
    if (nrow(df) == 0) { break; }
    names(df) = strsplit(dat[1], split="\\|")[[1]]
    gd$data[[labels[[i]][1]]] = df
  }
  
  if (length(gd$data) == 5) {
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
    lineup$startPos = sapply(strsplit(as.character(lineup$pos), split="-"), "[", 1)
    lineup.long = lineup[order(lineup$teamId, lineup$startPos),]
    lineup.wide = reshape(lineup.long, v.names = "playerId", timevar = "startPos", idvar = "teamId", direction="wide", drop=c("playerName", "bo", "pos"))
    def.pos = c("playerId.C", "playerId.1B", "playerId.2B", "playerId.3B", "playerId.SS", "playerId.LF", "playerId.CF", "playerId.RF")
    out = merge(x=out, y=lineup.wide[,c("teamId", def.pos)], by.x = "field_teamId", by.y = "teamId", all.x=TRUE)
    
    # Grab the defensive position of the batter
    out = merge(x=out, y=lineup[,c("playerId", "startPos")], by.x = "batterId", by.y = "playerId", all.x=TRUE)
    
    # Grab the batters and pitchers names
    out = merge(x=out, y=lkup[,c("playerId", "playerName")], by.x = "batterId", by.y = "playerId", all.x=TRUE)
    out = merge(x=out, y=lkup[,c("playerId", "playerName")], by.x = "pitcherId", by.y = "playerId", all.x=TRUE)
    
    # Clean up some column names
    idx = which(names(out) %in% c("inning.x", "event.x", "playerName.x", "playerName.y"))
    names(out)[idx] = c("inning", "event", "batterName", "pitcherName")
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
    require(plyr)
    out = ddply(out, ~half, updateDefense)  
    
    # Experimental -- I think we can filter out non-inning-all events now
    out = subset(out, !is.na(batterId))
    out = subset(out, !event %in% c("Game Advisory", "Defensive Switch", "Offensive sub", "Pitching Substitution"
                                    , "Defensive Sub", "Player Injured", "Ejection", "Umpire Substitution"))
    
    # Runs on play
    #    out$runsOnPlay = str_count(out$description, " scores.") + str_count(out$description, "homers")
    out$runsOnPlay = str_count(as.character(out$runnerMovement), ":T:")
    
    # runner movement, score, and outs
    out = ddply(out, ~inning + half, updateHalfInning)  
    # Once the non-PA related events have been removed, best to sort by ab_num, since the timestamp is occassionally missing!
    out = out[order(out$ab_num),]
    
    # number of baserunners
    out$startCode = as.numeric((1 * !is.na(out[, c("start1B", "start2B", "start3B")])) %*% c(1, 2, 4))
    out$endCode  = as.numeric((1 * !is.na(out[, c("end1B", "end2B", "end3B")])) %*% c(1, 2, 4))
    
    # Figure out who fielded the ball
    out$fielderId = getFielderId(out)
      
    # double-check -- should work for all normal plays, but fails on third out
  #  out$startBR = (1 * !is.na(out[, c("start1B", "start2B", "start3B")])) %*% c(1, 1, 1)
  #  out$endBR = (1 * !is.na(out[, c("end1B", "end2B", "end3B")])) %*% c(1, 1, 1)
  #  with(out, startBR + 1 - endBR == runsOnPlay + endOuts - startOuts)
    
    out$gameId = as.character(gd$gameId)
    out = out[order(out$ab_num),]
    
    # add some convenience calculation fields
    out = transform(out, isPA = !event %in% c("Defensive Indiff", "Stolen Base 2B", "Runner Out"))
    out = transform(out, isHit = event %in% c("Single", "Double", "Triple", "Home Run"))
    out = transform(out, isBIP = event != "Home Run" & !is.na(x) & !is.na(y))
    
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
  # IMPORTANT: Have to sort the data frame just in case
#  data = data[order(data$timestamp),]
  # Sometimes the timestamp is blank -- so sort these by AB num
  data = data[order(data$ab_num),]
  data$startOuts = 0 
  data$runsInInning = sum(data$runsOnPlay)
  data$runsITD = cumsum(c(0, data$runsOnPlay))[1:nrow(data)]
  data = transform(data, runsFuture = runsInInning - runsITD)
  data$start1B = NA
  data$start2B = NA
  data$start3B = NA
  data$end1B = NA
  data$end2B = NA
  data$end3B = NA
  leadoff.mv = getRunnerMovement(data[1, "runnerMovement"])
  data[1, "end1B"] = leadoff.mv["end1B"]
  data[1, "end2B"] = leadoff.mv["end2B"]
  data[1, "end3B"] = leadoff.mv["end3B"]
  if (nrow(data) > 1) {
    for (i in 2:nrow(data)) {
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
    rm.df = ddply(rm.df, ~ id, summarise, start = min(start), end = max(end))
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



updateDefense = function (data) {
  # IMPORTANT: Have to sort the data frame just in case
  # Have to sort by timestamp here, NOT by ab_num!
  data = data[order(data$timestamp),]
  idx = which(data$event %in% c("Defensive Switch", "Defensive Sub"))
  n = nrow(data)
  for(i in idx) {
    if (str_count(data[i,"description"], "(to catcher|as the catcher|playing catcher)")) { data[i:n,"playerId.C"] = data[i,"actionId"]; }
    if (str_count(data[i,"description"], "(to first base|as the first baseman|playing first base)")) { data[i:n,"playerId.1B"] = data[i,"actionId"]; }
    if (str_count(data[i,"description"], "(to second base|as the second baseman|playing second base)")) { data[i:n,"playerId.2B"] = data[i,"actionId"]; }
    if (str_count(data[i,"description"], "(to third base|as the third baseman|playing third base)")) { data[i:n,"playerId.3B"] = data[i,"actionId"]; }
    if (str_count(data[i,"description"], "(to shortstop|as the shortstop|playing shortstop)")) { data[i:n,"playerId.SS"] = data[i,"actionId"]; }
    if (str_count(data[i,"description"], "(to left field|as the left fielder|playing left field)")) { data[i:n,"playerId.LF"] = data[i,"actionId"]; }
    if (str_count(data[i,"description"], "(to center field|as the center fielder|playing center field)")) { data[i:n,"playerId.CF"] = data[i,"actionId"]; }
    if (str_count(data[i,"description"], "(to right field|as the right fielder|playing right field)")) { data[i:n,"playerId.RF"] = data[i,"actionId"]; }
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

################################################################################################
#
# Deprecated beyond this point
#
################################################################################################

#' @title getData
#' @aliases fetchData
#' 
#' @description Retrieves MLBAM GameDay files from the Internet
#' 
#' @details Given a valid gameId, this function will retrieve XML file from the 
#' GameDay server, and process them into a single data.frame
#' 
#' @param gameId A valid MLBAM gameId
#' @param use.cache logical indicating whether GameDay should be localled cached
#'  (TRUE by default)
#' 
#' @return a data.frame consisting of play-by-play data for that game
#' 
#' @export
#' @export fetchData.gameday
#' @examples
#' 
#' gd = gameday()
#' ds = getData(gd)
#' 


getData.gameday = function(gd, use.cache=TRUE) {
  filename = file.path(find.package("openWAR"), "data", gd$gameId, "processed.csv")
  if (!file.exists(filename)) {
    message(paste("...processed data for", gd$gameId, "is not in local directory"))
    out = try(fetchData.gameday(gd))
    if (class(out) == "try-error" | is.null(out)) {
      warning(paste("The data for", gd$gameId, "could not be processed"))
      return(NULL)
    } else {
      if (use.cache) {
        write.csv(out, filename, row.names=FALSE)
      }
      return(out)
    }
  } else {
    return(read.csv(filename))
  }
}




fetchData.gameday <- function(gd, use.cache=TRUE) {
  
  message(paste("...fetching GameDay data for", gd$gameId))
  
  allInnings <- getAllInnings(gd)
  gameEvents <- getGameEvents(gd)
  gameEvents$index <- rownames(gameEvents)
  
  # Test to make sure there is actually some data
  if (is.null(allInnings) | is.null(gameEvents)) {
    warning("This game contains no data")
    return(NULL)
  } else {
    
    #pull out the columns we want
    allInnings <- allInnings[,-c(12:14)]
    gameEvents <- gameEvents[,-c(2:7,9)]
    
    out <- merge(gameEvents,allInnings,by.x=c("gameId","num"),by.y=c("gameId","num"),all.x=TRUE)
    out <- out[order(as.numeric(as.character(out$index))),]
    
    #Running score
    out$home_team_runs[1]<-out$away_team_runs[1]<-0  
    for (i in 2:dim(out)[1]){if (is.na(out$home_team_runs[i])){
      out$home_team_runs[i]<-out$home_team_runs[i-1];out$away_team_runs[i]<-out$away_team_runs[i-1]}}
    
    #Remove game advisory, pitching subs, offensive subs, defensive subs
    #out<-out[!out$event%in%c("Game Advisory","Pitching Substitution","Offensive sub","Defensive Sub","Defensive Switch"),]
    
    
    #Correct for stolen bases, caught stealing, wild pitch, past ball, etc.  
    for (l in 2:nrow(out)){
      if (gregexpr("Stolen", out$event[l])[[1]][1]>0){
        #Start position is same as previous end position
        out$start1B[l]<-out$end1B[l-1]
        out$start2B[l]<-out$end2B[l-1]
        out$start3B[l]<-out$end3B[l-1]
        #end position depends on the base that was stolen
        if (gregexpr("2B",out$event[l])[[1]][1]>0){out$end1B[l]<-NA;out$end2B[l]<-out$start1B[l];out$end3B[l]<-out$start3B[l]}
        if (gregexpr("3B",out$event[l])[[1]][1]>0){out$end1B[l]<-out$start1B[l];out$end2B[l]<-NA;out$end3B[l]<-out$start2B[l]}
        out$o[l]<-out$o[l-1]
      }
      
      if (gregexpr("Caught",out$event[l])[[1]][1]>0){
        #Start position is same as previous end position
        out$start1B[l]<-out$end1B[l-1]
        out$start2B[l]<-out$end2B[l-1]
        out$start3B[l]<-out$end3B[l-1]
        #end position depends on the base that was stolen
        if (gregexpr("2B",out$event[l])[[1]][1]>0){out$end1B[l]<-NA;out$end2B[l]<-NA;out$end3B[l]<-out$start3B[l]}
        if (gregexpr("3B",out$event[l])[[1]][1]>0){out$end1B[l]<-out$start1B[l];out$end2B[l]<-NA;out$end3B[l]<-NA}
        out$o[l]<-out$o[l-1]
      }
      
      if (gregexpr("Wild Pitch",out$event[l])[[1]][1]>0){
#        out$inning[l] = out$inning[l-1]
        #Start position is same as previous end position
        out$start1B[l]<-out$start1B_WP[l+1]
        out$start2B[l]<-out$start2B_WP[l+1]
        out$start3B[l]<-out$start3B_WP[l+1]
        out$end1B[l]<-out$end1B_WP[l+1]
        out$end2B[l]<-out$end2B_WP[l+1]
        out$end3B[l]<-out$end3B_WP[l+1]
        out$o[l]<-out$o[l-1]
      }
    }
    
    
    
    for (j in 2:(dim(out)[1]-1)){
      if (!out[j-1,c("end1B")]%in%out[j,c("end1B","end2B","end3B","start1B","start2B","start3B")]  & is.na(out$end1B[j])){out$start1B[j]<-out$end1B[j]<-out$end1B[j-1]}
      if (!out[j-1,c("end2B")]%in%out[j,c("end1B","end2B","end3B","start1B","start2B","start3B")]  & is.na(out$end2B[j])){out$start2B[j]<-out$end2B[j]<-out$end2B[j-1]}
      if (!out[j-1,c("end3B")]%in%out[j,c("end1B","end2B","end3B","start1B","start2B","start3B")]  & is.na(out$end3B[j])){out$start3B[j]<-out$end3B[j]<-out$end3B[j-1]}
    }
    
    
   
    
    
    #Future runs data
    for (inning in as.numeric(sort(unique(out$inning)))){
      # print(inning)
                                                         top<-out[ !is.na(out$inning)  & out$inning==inning & out$half=="top",]
                                                         n<-dim(top)[1]
                                                         vec<-rep(NA,n)
                                                         vec[n]<-0
                                                         for (i in 1:(n-1)){vec[i]<-sum(!is.na(top$score1B)[(i+1):n],!is.na(top$score3B)[(i+1):n],!is.na(top$score2B)[(i+1):n])+sum(as.numeric(top$event=="Home Run")[(i+1):n])}
                                                         out[ !is.na(out$inning)  & out$inning==inning & out$half=="top",'futureRuns']<-vec
                                                         
                                                         bottom<-out[ !is.na(out$inning)  & out$inning==inning & out$half=="bottom",]
                                                         n<-dim(bottom)[1]
                                                         vec<-rep(NA,n)
                                                         vec[n]<-0
                                                         for (i in 1:(n-1)){vec[i]<-sum(!is.na(bottom$score1B)[(i+1):n],!is.na(bottom$score3B)[(i+1):n],!is.na(bottom$score2B)[(i+1):n])+sum(as.numeric(bottom$event=="Home Run")[(i+1):n])}
                                                         out[ !is.na(out$inning)  & out$inning==inning & out$half=="bottom",'futureRuns']<-vec
    }
    
    #Runs on a given play
    for (inning in as.numeric(sort(unique(out$inning)))){
      # print(inning)
                                                         top<-out[ !is.na(out$inning)  & out$inning==inning & out$half=="top",]
                                                         n<-dim(top)[1]
                                                         vec<-rep(NA,n)
                                                         for (i in 1:n){vec[i]<-sum(!is.na(top$score1B)[i],!is.na(top$score3B)[i],!is.na(top$score2B)[i])+sum(as.numeric(top$event=="Home Run")[i])}
                                                         out[ !is.na(out$inning)  & out$inning==inning & out$half=="top",'runsOnPlay']<-vec
                                                         
                                                         bottom<-out[ !is.na(out$inning)  & out$inning==inning & out$half=="bottom",]
                                                         n<-dim(bottom)[1]
                                                         vec<-rep(NA,n)
                                                         for (i in 1:n){vec[i]<-sum(!is.na(bottom$score1B)[i],!is.na(bottom$score3B)[i],!is.na(bottom$score2B)[i])+sum(as.numeric(bottom$event=="Home Run")[i])}
                                                         out[ !is.na(out$inning)  & out$inning==inning & out$half=="bottom",'runsOnPlay']<-vec
    }
    
    
    
    
    
    #Add in hit locations
  #  inningHit<- getGameDayXML(gd$gameId, type = "inning_hit.xml")
  #  hitLocs<-getNodeSet(inningHit$doc$children$hitchart,"//hip")
    
    hitLocs = getNodeSet(gd$xml[["inning_hit.xml"]], "//hip")
    tempHitList<-list()
    for (i in 1:length(hitLocs))
    {
      tempHitList[[i]]<-(xmlAttrs(hitLocs[[i]]))[1:14]
    }
    
    hitList.df<-as.data.frame(do.call(rbind,tempHitList))[,c("des","x","y","batter","pitcher","inning")]
    names(hitList.df)[1]<-"event"
    
    out<-merge(out,hitList.df,by.x=c("event","batter","pitcher","inning"),by.y=c("event","batter","pitcher","inning"),all.x=TRUE)
    out<-out[order(as.numeric(out$index)),]
    
    
    # Convert columns to numerics
    cols = c("num", "home_team_runs", "away_team_runs", "b1", "b2", "b3", "rbi", "index"
             , "b", "s", "o", "timestamp", "batter", "pitcher", "start1B", "start2B"
             , "start3B", "end1B", "end2B", "end3B", "score1B", "score2B", "score3B"
             , "inning", "x", "y", "field_teamId")
    colIds = which(names(out) %in% cols)
    for (colId in colIds) {
      #      cat(colId)
      out[,colId] = as.numeric(as.character(out[,colId]))
    }
 
    # Way too ugly!!
#    for (i in 1:dim(out)[1]){out$bc_before[i]<-as.numeric(!is.na(c(out$start1B[i],out$start2B[i],out$start3B[i])))%*%c(1,2,4)}
#    for (i in 1:dim(out)[1]){out$bc_after[i]<-as.numeric(!is.na(c(out$end1B[i],out$end2B[i],out$end3B[i])))%*%c(1,2,4)}
    
    out$bc_before = (1 * !is.na(out[, c("start1B", "start2B", "start3B")])) %*% c(1, 2, 4)
    out$bc_after  = (1 * !is.na(out[, c("end1B", "end2B", "end3B")])) %*% c(1, 2, 4)
    
    # Want futureRuns to include the current play
    out = transform(out, futureRuns = futureRuns + runsOnPlay)
    
    #outs before and outs after
    ind<-!out$event%in%c("Game Advisory","Pitching Substitution","Offensive sub","Defensive Sub","Defensive Switch")
    out$endOuts<-out$startOuts<-as.numeric(as.character(out$o))
    out$startOuts[1]<-0
    for (i in 2:dim(out[ind,])[1]){out$startOuts[ind][i]<-as.numeric(as.character(out$endOuts[ind][i-1]))}
    out$startOuts[out$startOuts==3]<-0
    
    
    #  summary(out)
    
    return(out)
  }
}

##########################################################################################################

#' @title summary
#' 
#' @description Provides a brief summary of a GameDay object
#' 
#' @details Quickly summarizes some properties of the game
#' 
#' @return void
#' 
#' @export
#' @examples
#' gd = gameday()
#' summary(gd)
#' 

summary.gameday = function (gd) {
  cat(paste("\nSummarizing downloaded data for", gd$gameId))
  home_teamId = unique(as.character(gd$ds$home_teamId))
  away_teamId = unique(as.character(gd$ds$away_teamId))
  scores = ddply(gd$ds[,c("field_teamId", "runsOnPlay")], ~field_teamId, summarise, R = sum(runsOnPlay))
  home_score = subset(scores, field_teamId == away_teamId)$R
  away_score = subset(scores, field_teamId == home_teamId)$R
  if (home_score > away_score) { 
    cat(paste("\n...", home_teamId, "beat", away_teamId, "by a score of", home_score, "-", away_score))
  } else {
    cat(paste("\n...", away_teamId, "beat", home_teamId, "by a score of", away_score, "-", home_score))
  }
  cat(paste(" in", max(gd$ds$inning, na.rm=TRUE), "innings"))
  cat(paste("\n...there were", max(gd$ds$ab_num, na.rm=TRUE), "plate appearances in the game"))  
}


####################################################################################
#
#    No user interaction beyond this point
#
####################################################################################


getHalfInning<-function(gd, inning, half){
#  inningData <- getGameDayXML(gameId, type = "inning_all.xml")  
#  aa<-getNodeSet(inningData$doc$children$game[[inning]][[half]], "//atbat")
  
  inningData = gd$xml[["inning_all.xml"]]
  xpath = paste("//inning[@num=", inning, "]/", half, sep="")
#  message(xpath)
  nodeSet = getNodeSet(inningData, paste(xpath, "//atbat", sep=""))
  aa = nodeSet
  
  if (!is.null(aa)) {
    
    tempList<-list()
    tempRunnerList<-list()
    for (i in 1:length(aa))
    {
      tempList[[i]]<-(xmlAttrs(aa[[i]]))[1:14]
    }
    
    df<-as.data.frame(do.call(rbind,tempList))
    df$start1B<-NA
    df$start2B<-NA
    df$start3B<-NA
    df$end1B<-NA
    df$end2B<-NA
    df$end3B<-NA
    df$score1B<-NA
    df$score2B<-NA
    df$score3B<-NA
    
    #For wild pitches
    df$start1B_WP<-NA
    df$start2B_WP<-NA
    df$start3B_WP<-NA
    df$end1B_WP<-NA
    df$end2B_WP<-NA
    df$end3B_WP<-NA
    
    
    for (i in 1:length(aa))
    {
      runnersNodeSet <- getNodeSet(xmlClone(aa[[i]]), "//runner")
      
      if (length(runnersNodeSet)>0){
        for (q in 1:length(runnersNodeSet)) { 
#          print(q)
               #Pull out wild pitches and stolen bases and balks etc.
               if ( gregexpr('Stolen',xmlAttrs(runnersNodeSet[[q]])[['event']])[[1]][1]>0){runnersNodeSet[[q]]<-NA}
                                            
        }
      }
      
      
      #!is.null(aa[[i]][['runner']]) 
      if (length(runnersNodeSet)>0){
        
        for (q in 1:length(runnersNodeSet)) {
  #        print(q)
                                            
           #!is.na(xmlAttrs(runnersNodeSet[[q]])[['id']])
          if (class(runnersNodeSet[[q]])[1] %in% c("XMLNode", "XMLInternalElementNode")) {
            if (!gregexpr('Wild Pitch',xmlAttrs(runnersNodeSet[[q]])[['event']])[[1]][1]>0){
            if (xmlAttrs(runnersNodeSet[[q]])[['start']]=="1B"){df$start1B[i]<-xmlAttrs(runnersNodeSet[[q]])[['id']]}
            if (xmlAttrs(runnersNodeSet[[q]])[['start']]=="2B"){df$start2B[i]<-xmlAttrs(runnersNodeSet[[q]])[['id']]}
            if (xmlAttrs(runnersNodeSet[[q]])[['start']]=="3B"){df$start3B[i]<-xmlAttrs(runnersNodeSet[[q]])[['id']]}
            if (xmlAttrs(runnersNodeSet[[q]])[['end']]=="1B"){df$end1B[i]<-xmlAttrs(runnersNodeSet[[q]])[['id']]}
            if (xmlAttrs(runnersNodeSet[[q]])[['end']]=="2B"){df$end2B[i]<-xmlAttrs(runnersNodeSet[[q]])[['id']]}
            if (xmlAttrs(runnersNodeSet[[q]])[['end']]=="3B"){df$end3B[i]<-xmlAttrs(runnersNodeSet[[q]])[['id']]}
                                                  
            if (c('score')%in%names(xmlAttrs(runnersNodeSet[[q]]))) {
              if (xmlAttrs(runnersNodeSet[[q]])[['start']]=="1B"){df$score1B[i]<-xmlAttrs(runnersNodeSet[[q]])[['score']]}
              if (xmlAttrs(runnersNodeSet[[q]])[['start']]=="2B"){df$score2B[i]<-xmlAttrs(runnersNodeSet[[q]])[['score']]}
              if (xmlAttrs(runnersNodeSet[[q]])[['start']]=="3B"){df$score3B[i]<-xmlAttrs(runnersNodeSet[[q]])[['score']]}
            }
                                                }
            if (gregexpr('Wild Pitch', xmlAttrs(runnersNodeSet[[q]])[['event']])[[1]][1]>0) {
              if (xmlAttrs(runnersNodeSet[[q]])[['start']]=="1B"){df$start1B_WP[i]<-xmlAttrs(runnersNodeSet[[q]])[['id']]}
              if (xmlAttrs(runnersNodeSet[[q]])[['start']]=="2B"){df$start2B_WP[i]<-xmlAttrs(runnersNodeSet[[q]])[['id']]}
              if (xmlAttrs(runnersNodeSet[[q]])[['start']]=="3B"){df$start3B_WP[i]<-xmlAttrs(runnersNodeSet[[q]])[['id']]}
              if (xmlAttrs(runnersNodeSet[[q]])[['end']]=="1B"){df$end1B_WP[i]<-xmlAttrs(runnersNodeSet[[q]])[['id']]}
              if (xmlAttrs(runnersNodeSet[[q]])[['end']]=="2B"){df$end2B_WP[i]<-xmlAttrs(runnersNodeSet[[q]])[['id']]}
              if (xmlAttrs(runnersNodeSet[[q]])[['end']]=="3B"){df$end3B_WP[i]<-xmlAttrs(runnersNodeSet[[q]])[['id']]}
            }
          }
        }
      }
    }
    
    df[,c(c("start1B","start2B","start3B"),c("end1B","end2B","end3B"))]
    df
    df$inning<-inning
    df$half<-half
    df$gameId <- gd$gameId
    return(df)
  } else {
    return(NULL)
  }
}


getAllInnings<-function(gd) {
    
  fielders = gd$xml[["bis_boxscore.xml"]]
  xmlFielders <- getNodeSet(fielders,"//batting")
  t.fielders.list <- list()
  batterPos.list <- list()
  for (k in 1:2){
    #i-th fielders
    fielders.list<-list()
      
    test.error<-0
    i<-1
    while (class(test.error) != "try-error") {
      fielder = xmlFielders[[k]][[i]]
      if (!is.null(fielder)) {
        fielders.list[[i]] <- test.error <- try(c(xmlAttrs(fielder)[c("id","pos","name","bo")]))
        i<-i+1
      } else {
        break;
      }
    }
    dfFielders<-batterPos.list[[k]] <- as.data.frame(do.call(rbind,fielders.list))
    dfFielders<-dfFielders[-dim(dfFielders)[1],]
    
    # Drop pitcher and DH
    dfFielders <- dfFielders[!dfFielders$pos %in% c("DH","P","PH") & as.numeric(as.character(dfFielders$bo)) %% 100 == 0,]
    dfFielders$pos <- factor(dfFielders$pos, levels=c("C","1B","2B","3B","SS","RF","CF","LF"))
    dfFielders[order(dfFielders$pos),]
    t.fielders <- t(dfFielders)[1,]
    
    # Since plays are listed in terms of the offense, these team indicators need to also be in terms of the offense. 
    t.fielders[9] <- toupper(substring(xmlAttrs(xmlFielders[[3-k]])["team_flag"],1,1))
    names(t.fielders) <- c("C","B1","B2","B3","SS","RF","CF","LF","team")
    t.fielders.list[[k]] <- t.fielders
  }
  
  fielders.for.merg <- do.call(rbind,t.fielders.list)
  
  ######################################################
  
  inningData = gd$xml[["inning_all.xml"]]
  numInnings = length(getNodeSet(inningData, "//inning"))
  
  # Test to make sure there is actually some data
  if (numInnings < 2) {
    warning("This game contains no data")
    return(NULL)
  } else {
    
    inningList <- list()
    #inningList[[1]]<-try(getInning(gameId,1,'top',inningData))
    #names(inningList[[1]])[14]<-"score"
    
    j=1
    for (half in c("top", "bottom")) {
      for (inn in 1:numInnings){
        #while(class(inningList[[inn-1]])!='try-error'){    print(inn)            
    
        half.inning = try(getHalfInning(gd, inn, half))
        if ( class(half.inning) != 'try-error') {
  #        names(inningList[[inn]])[14]<-"score"
          inningList[[j]] = half.inning 
          j = j + 1
        }
      }
    }
    
    
    df <- do.call(rbind,inningList)
    
    # WHY????
  #  top<-top[-dim(top)[1],]
    
    # bind on starting fielders
    fielders.for.merg <- as.data.frame(fielders.for.merg)
    top <- cbind(subset(df, half == "top"), as.data.frame((fielders.for.merg[fielders.for.merg$team=="H",1:8])))
    bottom <- cbind(subset(df, half == "bottom"), as.data.frame((fielders.for.merg[fielders.for.merg$team=="A",1:8])))
    
    out<-rbind(top,bottom)
    
    # Convert columns to character vectors
    cols = c("batter", "pitcher", "stand", "C", "B1", "B2", "B3", "SS", "LF", "CF", "RF")
    colIds = which(names(out) %in% cols)
    for (colId in colIds) {
      #      cat(colId)
      out[,colId] = as.character(out[,colId])
    }
    
    # Add the batter's name
    batterPos <- do.call(rbind, batterPos.list)
    out <- merge(out, batterPos, by.x="batter", by.y="id", all.x=TRUE)
    names(out)[which(names(out) == "name")] = "batterName"
    
    # Add the pitcher's name
    out <- merge(out, batterPos[,c("id", "name")], by.x="pitcher", by.y="id", all.x=TRUE)
    names(out)[which(names(out) == "name")] = "pitcherName"
    
    return(out)
  }
}





getGameEvents <- function (gd) {
#  test = getGameDayXML(gd$gameId, type = "game_events.xml")
#  aa<-getNodeSet(test$doc$children$game,c("//atbat","//action"))
  
  # Equivalent
  test = gd$xml[["game_events.xml"]]
  aa <- getNodeSet(test, c("//atbat", "//action"))
  
  # Test to make sure there is actually some data
  if (length(aa) == 0) {
    warning("This game contains no data")
    return(NULL)
  } else {
    
    # Which columns do you want to capture?
    cols = c("num", "b", "s", "o", "start_tfs", "batter", "pitcher", "des", "des_es", "event", "score", "home_team_runs"
             , "away_team_runs", "b1", "b2", "b3", "rbi")
    # Create an empty data.frame
    gr = as.data.frame(matrix(NA, nrow=length(aa), ncol=length(cols)))
    names(gr) = cols
    
    # Fill it by row
    for (i in 1:length(aa)){
      if ("num"%in%names(xmlAttrs(aa[[i]]))) {gr$num[i]<-xmlAttrs(aa[[i]])['num']}
      if ("b"%in%names(xmlAttrs(aa[[i]]))) {gr$b[i]<-xmlAttrs(aa[[i]])['b']}
      if ("s"%in%names(xmlAttrs(aa[[i]]))) {gr$s[i]<-xmlAttrs(aa[[i]])['s']}
      if ("o"%in%names(xmlAttrs(aa[[i]]))) {gr$o[i]<-xmlAttrs(aa[[i]])['o']}
      if ("start_tfs"%in%names(xmlAttrs(aa[[i]]))) {gr$start_tfs[i]<-xmlAttrs(aa[[i]])['start_tfs']}
      if ("batter"%in%names(xmlAttrs(aa[[i]]))) {gr$batter[i]<-xmlAttrs(aa[[i]])['batter']}
      if ("pitcher"%in%names(xmlAttrs(aa[[i]]))) {gr$pitcher[i]<-xmlAttrs(aa[[i]])['pitcher']}
      if ("des"%in%names(xmlAttrs(aa[[i]]))) {gr$des[i]<-xmlAttrs(aa[[i]])['des']}
      if ("des_es"%in%names(xmlAttrs(aa[[i]]))) {gr$des_es[i]<-xmlAttrs(aa[[i]])['des_es']}
      if ("event"%in%names(xmlAttrs(aa[[i]]))) {gr$event[i]<-xmlAttrs(aa[[i]])['event']}
      if ("score"%in%names(xmlAttrs(aa[[i]]))) {gr$score[i]<-xmlAttrs(aa[[i]])['score']}
      if ("home_team_runs"%in%names(xmlAttrs(aa[[i]]))) {gr$home_team_runs[i]<-xmlAttrs(aa[[i]])['home_team_runs']}
      if ("away_team_runs"%in%names(xmlAttrs(aa[[i]]))) {gr$away_team_runs[i]<-xmlAttrs(aa[[i]])['away_team_runs']}
      if ("b1"%in%names(xmlAttrs(aa[[i]]))) {gr$b1[i]<-xmlAttrs(aa[[i]])['b1']}
      if ("b2"%in%names(xmlAttrs(aa[[i]]))) {gr$b2[i]<-xmlAttrs(aa[[i]])['b2']}
      if ("b3"%in%names(xmlAttrs(aa[[i]]))) {gr$b3[i]<-xmlAttrs(aa[[i]])['b3']}
      if ("rbi"%in%names(xmlAttrs(aa[[i]]))) {gr$rbi[i]<-xmlAttrs(aa[[i]])['rbi']}
    }
    
    gr$gameId <- gd$gameId
    
    #Stadium
  #  stad = getGameDayXML(gameId, type = "game.xml")
    stad = gd$xml[["game.xml"]]
    venueId <- as.numeric(xmlAttrs(getNodeSet(stad,"//stadium")[[1]])["id"])
    stadium <- xmlAttrs(getNodeSet(stad,"//stadium")[[1]])["name"]
    game_type <- xmlAttrs(getNodeSet(stad, "//game")[[1]])["type"]
    
    gr$venueId <- venueId
    gr$stadium <- stadium
    gr$game_type <- game_type
    return(gr)
  }
}

  
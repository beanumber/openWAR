#' @title Convenience class for dealing with MLBAM GameDay files 
#' 
#' @description Each gameId corresponds to one object of class \code{\link{gameday}}
#' 
#' @details A \code{\link{gameday}} object is a list containing 4 elements: \code{gameId}, \code{base}, \code{url}, and \code{ds}.  \code{gameId} is the 
#' unique identifier for a game, \code{base} contains the base URL corresponding the the \code{gameId}, \code{url} contains the sub urls pointing to different 
#' data sets that are combined together to create an object of class \code{\link{GameDayPlays}} containing play-by-play data.  The \code{ds} slot of the list 
#' contains the \code{\link{GameDayPlays}} object of play-by-play data.  
#' 
#' @param gameId A valid MLBAM gameId
#' @param ... currently ignored
#' 
#' @return An object of class \code{\link{gameday}}, which consists of a list containing
#' \item{gameId}{The ID of the game (e.g. 'gid_2012_08_12_atlmlb_nynmlb_1')}
#' \item{base}{The base URL for the game (e.g. 'http://gd2.mlb.com/components/game/mlb/year_2012/month_08/day_12/')}
#' \item{url}{A character vector containing the names of the XML files downloaded from the GameDay server}
#' \item{ds}{The \code{\link{GameDayPlays}} object for use with openWAR containing all 
#' play-by-play information for the game indicated by the gameId}
#' 
#' @export
#' @examples
#' 
#' exampleGameday <- gameday(gameId = 'gid_2012_08_12_atlmlb_nynmlb_1')
#' # Display GameId
#' exampleGameday$gameId
#' # Summary of the different types fo game events for gameId = 'gid_2012_08_12_atlmlb_nynmlb_1'
#' summary(exampleGameday$ds$event)
#' plot(exampleGameday$ds)

gameday = function(gameId = "gid_2012_08_12_atlmlb_nynmlb_1", ...) {
    # Check to make the gameId is at least potentially valid
    if (nchar(gameId) != 30) {
        stop("This is not a valid MLBAM gameId!")
    }
    yyyy <- substring(gameId, 5, 8)
    mm <- substring(gameId, 10, 11)
    dd <- substring(gameId, 13, 14)
    
    # Base URL
    base = paste("http://gd2.mlb.com/components/game/mlb/year_", yyyy, "/month_", mm, "/day_", dd, "/", sep = "")
    message(gameId)
    
    gd = list(gameId = gameId, base = base)
    class(gd) = "gameday"
    gd$url = getURLs.gameday(gd)
    # gd$xml = getXML.gameday(gd) if (!is.null(gd$xml)) { gd$ds = getData.gameday(gd) }
    try(gd$ds <- readData.gameday(gd))
    return(gd)
}

############################################################################################### Utility functions

#' @title getURLs.gameday
#' @aliases getXML.gameday
#' 
#' @description Convenience function for returning data related to a game
#' 
#' @details Given a gameId string, this function can return several URLs
#' 
#' @param gd A \code{gameday} object
#' 
#' @return a vector of URLs


getURLs.gameday = function(gd) {
    url = NULL
    url["bis_boxscore.xml"] = paste(gd$base, gd$gameId, "/bis_boxscore.xml", sep = "")
    url["inning_all.xml"] = paste(gd$base, gd$gameId, "/inning/inning_all.xml", sep = "")
    url["inning_hit.xml"] = paste(gd$base, gd$gameId, "/inning/inning_hit.xml", sep = "")
    url["game.xml"] = paste(gd$base, gd$gameId, "/game.xml", sep = "")
    url["game_events.xml"] = paste(gd$base, gd$gameId, "/game_events.xml", sep = "")
    return(url)
}

#' @title get game data
#' 
#' @description Grabs XML files from MLBAM and converts to data.frames in memory
#' 
#' @details Grabs XML files from MLBAM and converts to data.frames in memory
#' 
#' @param gd A gameday object
#' 
#' @return A gameday object
#'
#' @importFrom Sxslt xsltApplyStyleSheet
#' @importFrom XML saveXML
#' @importFrom stringr str_split
#' @importFrom stringr str_count
#' @import dplyr
#' 
#' @export
#' @examples
#' 
#' library(openWAR)
#' gd <- gameday()

readData.gameday = function(gd) {
  
    # First, build gd$data as a list of data frames
#    dirname = file.path(find.package("openWAR"))
    labels = do.call("rbind", strsplit(basename(gd$url), split = "\\."))[, 1]
    for (i in 1:length(labels)) {
      # Find the XSLT template
      xslt.filename <- paste(labels[i], ".xsl", sep = "")
      xsl <- system.file("xsl", xslt.filename, package = "openWAR")
#        xsl = paste(dirname, "/xsl/", labels[i], ".xsl", sep = "")
      
      # Use the shell command 'xsltproc' cmd = paste('xsltproc', xsl, gd$url[i], sep=' ') dat = try(system(cmd, intern=TRUE))
      # Alternative within R apply the stylesheet to the XML
      dat = stringr::str_split(XML::saveXML(Sxslt::xsltApplyStyleSheet(gd$url[i], xsl)$doc), "\n")[[1]]
      # remove the xml description on the first line
      dat = dat[-1]
      # remove any blank lines
      dat = dat[dat != ""]
      
      if (!is.null(attr(dat, "status"))) {
        break
      }
      df = as.data.frame(do.call("rbind", strsplit(dat[-1], split = "\\|")))
      if (nrow(df) == 0) {
        break
      }
      names(df) = strsplit(dat[1], split = "\\|")[[1]]
      gd$data[[labels[[i]][1]]] = df
    }
    
    if (length(gd$data) == 5) {
      # If any timestamps are missing, set them equal to the previous timstamp
      missing.idx = which(gd$data$game_events$timestamp == "")
      gd$data$game_events$timestamp[missing.idx] = gd$data$game_events$timestamp[missing.idx - 1]
      
      # Now, merge the data frames into one large data frame
      out <- merge(x = gd$data$inning_all, y = gd$data$inning_hit[, c("inning", "batterId", "pitcherId", "event", "x", "y")], 
                   by = c("inning", "batterId", "pitcherId", "event"), all.x = TRUE)
      out <- merge(x = gd$data$game_events, y = out, by = "ab_num", all.x = TRUE)
      out <- cbind(out, gd$data$game)
      
      # Make sure R understands that timestamp is a date
      out <- dplyr::mutate_(out, timestamp = ~as.character(strptime(timestamp.x, format = "%Y-%m-%dT%H:%M:%S")))
      # Make sure that the runnerMovement is not a factor
      out <- dplyr::mutate_(out, runnerMovement = ~as.character(runnerMovement))
      
      # Merge again to get the starting fielders
      out <- dplyr::mutate_(out, field_teamId = ~ifelse(half == "top", as.character(home_teamId), as.character(away_teamId)))
      # Create a lookup for the player's name
      lkup = gd$data$bis_boxscore
      lkup = lkup[!duplicated(lkup$playerId), ]
      
      # suppressWarnings for na
      lkup <- dplyr::mutate_(lkup, bo = ~suppressWarnings(as.numeric(as.character(bo))))
      
      # Weed out pitchers
      lineup = dplyr::filter_(lkup, ~bo %% 100 == 0)
      lineup$batterPos = sapply(strsplit(as.character(lineup$pos), split = "-"), "[", 1)
#      lineup.long = lineup[order(lineup$teamId, lineup$batterPos), ]
      lineup.long <- dplyr::arrange_(lineup, ~teamId, ~batterPos)
      lineup.wide <- reshape(lineup.long, v.names = "playerId", timevar = "batterPos", idvar = "teamId", direction = "wide", 
                             drop = c("playerName", "bo", "pos"))
      def.pos <- c("playerId.C", "playerId.1B", "playerId.2B", "playerId.3B", "playerId.SS", "playerId.LF", "playerId.CF", 
                   "playerId.RF")
      out <- merge(x = out, y = lineup.wide[, c("teamId", def.pos)], by.x = "field_teamId", by.y = "teamId", all.x = TRUE)
      
      # Grab the defensive position of the batter
      out <- merge(x = out, y = lineup[, c("playerId", "batterPos")], by.x = "batterId", by.y = "playerId", all.x = TRUE)
      
      # Grab the batters and pitchers names
      out <- merge(x = out, y = lkup[, c("playerId", "playerName")], by.x = "batterId", by.y = "playerId", all.x = TRUE)
      out <- merge(x = out, y = lkup[, c("playerId", "playerName")], by.x = "pitcherId", by.y = "playerId", all.x = TRUE)
      
      # Clean up some column names
      idx <- which(names(out) %in% c("inning.x", "event.x", "batterPos", "playerName.x", "playerName.y"))
      names(out)[idx] = c("inning", "event", "batterPos", "batterName", "pitcherName")
      out = out[, !names(out) %in% c("inning.y", "event.y", "timestamp.x", "timestamp.y")]
      
      # Convert columns to numerics
      cols <- c("ab_num", "balls", "strikes", "endOuts", "batterId", "pitcherId", "actionId", "home_teamId", "away_teamId", 
                "venueId", "inning", "x", "y", "playerId.1B", "playerId.2B", "playerId.3B", "playerId.C", "playerId.CF", "playerId.LF", 
                "playerId.P", "playerId.RF", "playerId.SS")
      colIds = which(names(out) %in% cols)
      for (colId in colIds) {
        # cat(colId)
        out[, colId] = as.numeric(as.character(out[, colId]))
      }
      
      # Update fielders for defensive switches
      out = makeSubstitutions(out)
      # Sometimes data errors result in the batter's defensive position not getting set.  Set it to UN manually.
      missing.idx = which(is.na(out$batterPos) & out$batterId > 0)
      out$batterPos[missing.idx] = "UN"
      
      # Experimental -- I think we can filter out non-inning-all events now
      out = dplyr::filter_(out, ~!is.na(batterId))
      out = dplyr::filter_(out, ~!event %in% c("Game Advisory", "Defensive Switch", "Offensive sub", "Pitching Substitution", "Defensive Sub", 
                                      "Player Injured", "Ejection", "Umpire Substitution"))
      
      # Runs on play out$runsOnPlay = str_count(out$description, ' scores.') + str_count(out$description, 'homers')
      out$runsOnPlay = stringr::str_count(as.character(out$runnerMovement), ":T:")
      
      # runner movement, score, and outs out = ddply(out, ~inning + half, updateHalfInning)
      out <- dplyr::do_(dplyr::group_by_(out, ~inning, ~half), ~updateHalfInning(.))
      # Once the non-PA related events have been removed, best to sort by ab_num, since the timestamp is occassionally missing!
#      out = out[order(out$ab_num), ]
      out <- arrange_(out, ~ab_num)
      
      # number of baserunners
      out$startCode <- as.numeric((1 * (!is.na(as.data.frame(out)[, c("start1B", "start2B", "start3B")]))) %*% c(1, 2, 4))
      out$endCode <- as.numeric((1 * (!is.na(as.data.frame(out)[, c("end1B", "end2B", "end3B")]))) %*% c(1, 2, 4))
      
      # Figure out who fielded the ball
      out$fielderId = getFielderId(out)
      
      # double-check -- should work for all normal plays, but fails on third out out$startBR = (1 * !is.na(out[, c('start1B',
      # 'start2B', 'start3B')])) %*% c(1, 1, 1) out$endBR = (1 * !is.na(out[, c('end1B', 'end2B', 'end3B')])) %*% c(1, 1, 1)
      # with(out, startBR + 1 - endBR == runsOnPlay + endOuts - startOuts)
      
      out$gameId = as.character(gd$gameId)
#      out = out[order(out$ab_num), ]
      out <- arrange_(out, ~ab_num)
      
      # add some convenience calculation fields
      out <- dplyr::mutate_(out, isPA = ~!event %in% c("Defensive Indiff", "Stolen Base 2B", "Runner Out"))
      out <- dplyr::mutate_(out, isAB = ~isPA & !event %in% c("Walk", "Intent Walk", "Hit By Pitch", "Sac Fly", "Sac Bunt"))
      out <- dplyr::mutate_(out, isHit = ~event %in% c("Single", "Double", "Triple", "Home Run"))
      out <- dplyr::mutate_(out, isBIP = ~event != "Home Run" & !is.na(x) & !is.na(y))
      
      # translate the coordinates so that home plate is (0,0) and second base is (0, 127' 3 3/8')
      out = recenter(out)
    } else {
      warning("Game data is no bueno -- most likely a rainout")
      out = NULL
    }
    out <- ungroup(out)
    class(out) <- c("GameDayPlays", class(out))
    return(out)
}

#' @title updataHalfInning
#' 
#' @description updateHalfInning
#' 
#' @param dat a \code{GameDayPlays} object containing information about a single inning.
#' 
#' @return A gameday object
#'

updateHalfInning <- function(dat) {
#  cat(print(paste(dat$inning[1], dat$half[1])))
    # cat(print('new inning')) 
    # IMPORTANT: Have to sort the data frame just in case data = data[order(data$timestamp),] 
    # Sometimes the timestamp is blank -- so sort these by AB num
#    dat = dat[order(dat$ab_num), ]
    dat <- arrange_(dat, ~ab_num)
    dat$startOuts = 0
    dat$runsInInning = sum(dat$runsOnPlay)
    dat$runsITD = cumsum(c(0, dat$runsOnPlay))[1:nrow(dat)]
    dat = dplyr::mutate_(dat, runsFuture = ~runsInInning - runsITD)
    dat$start1B = as.character(NA)
    dat$start2B = as.character(NA)
    dat$start3B = as.character(NA)
    dat$end1B = as.character(NA)
    dat$end2B = as.character(NA)
    dat$end3B = as.character(NA)
#  cat(print(dat$runnerMovement[1]))
    leadoff.mv = getRunnerMovement(dat[1, "runnerMovement"])
    dat[1, "end1B"] = leadoff.mv["end1B"]
    dat[1, "end2B"] = leadoff.mv["end2B"]
    dat[1, "end3B"] = leadoff.mv["end3B"]
    if (nrow(dat) > 1) {
        for (i in 2:nrow(dat)) {
            # cat(print(i))
            dat[i, "startOuts"] = dat[i - 1, "endOuts"]
            dat[i, "start1B"] = dat[i - 1, "end1B"]
            dat[i, "start2B"] = dat[i - 1, "end2B"]
            dat[i, "start3B"] = dat[i - 1, "end3B"]
            runner.mv = getRunnerMovement(dat[i, "runnerMovement"])
            if (!is.na(runner.mv["end1B"])) {
                dat[i, "end1B"] = runner.mv["end1B"]
            } else if (!is.na(dat[i, "start1B"]) & is.na(runner.mv["start1B"])) {
                dat[i, "end1B"] = dat[i, "start1B"]
            }
            if (!is.na(runner.mv["end2B"])) {
                dat[i, "end2B"] = runner.mv["end2B"]
            } else if (!is.na(dat[i, "start2B"]) & is.na(runner.mv["start2B"])) {
                dat[i, "end2B"] = dat[i, "start2B"]
            }
            if (!is.na(runner.mv["end3B"])) {
                dat[i, "end3B"] = runner.mv["end3B"]
            } else if (!is.na(dat[i, "start3B"]) & is.na(runner.mv["start3B"])) {
                dat[i, "end3B"] = dat[i, "start3B"]
            }
            # Always end the inning with nobody on if (dat[i, 'endOuts'] == 3) { dat[i, 'end1B'] = NA dat[i, 'end2B'] = NA dat[i,
            # 'end3B'] = NA }
        }
    }
    # Force the bases empty at the end of every inning
    dat[nrow(dat), "end1B"] = NA
    dat[nrow(dat), "end2B"] = NA
    dat[nrow(dat), "end3B"] = NA
    dat$outsInInning = sum(dat$endOuts - dat$startOuts)
    return(dat)
}

#' @title getRunnerMovement
#' 
#' @param x a character vector of runner movements
#' 
#' @importFrom stringr str_split
#' @import dplyr
#'
getRunnerMovement = function(x) {
    # runner movement
    y = as.character(x)
#    cat(print(y))
    rm.vec = rep(NA, 6)
    names(rm.vec) = c("start1B", "start2B", "start3B", "end1B", "end2B", "end3B")
    if (!is.na(x) & nchar(y[1]) > 0) {
        z = substr(y, start = 2, stop = (nchar(y) - 1))
        rm.df <- as.data.frame(do.call("rbind", stringr::str_split(unlist(stringr::str_split(z, "\\]\\[")), ":")), stringsAsFactors = FALSE)
        
#        cat(print(class(rm.df)))
#        cat(print(names(rm.df)))
        
        names(rm.df) = c("id", "start", "end", "event")
        
        # In case a runner moved twice during the at-bat, for now just concentrate on where he ended up
        rm.df$end = ifelse(rm.df$end == "", "4B", rm.df$end)
        # rm.df = ddply(rm.df, ~ id, summarise, start = min(start), end = max(end))
        
        rm.df <- dplyr::summarize(dplyr::group_by_(rm.df, ~id), start = min(start), end = max(end))
        
        rm.df$end = ifelse(rm.df$end == "4B", "", rm.df$end)
        if (nrow(dplyr::filter_(rm.df, ~start == "1B")) > 0) {
            rm.vec["start1B"] = dplyr::filter_(rm.df, ~start == "1B")$id
        }
        if (nrow(dplyr::filter_(rm.df, ~start == "2B")) > 0) {
            rm.vec["start2B"] = dplyr::filter_(rm.df, ~start == "2B")$id
        }
        if (nrow(dplyr::filter_(rm.df, ~start == "3B")) > 0) {
            rm.vec["start3B"] = dplyr::filter_(rm.df, ~start == "3B")$id
        }
        if (nrow(dplyr::filter_(rm.df, ~end == "1B")) > 0) {
            rm.vec["end1B"] = dplyr::filter_(rm.df, ~end == "1B")$id
        }
        if (nrow(dplyr::filter_(rm.df, ~end == "2B")) > 0) {
            rm.vec["end2B"] = dplyr::filter_(rm.df, ~end == "2B")$id
        }
        if (nrow(dplyr::filter_(rm.df, ~end == "3B")) > 0) {
            rm.vec["end3B"] = dplyr::filter_(rm.df, ~end == "3B")$id
        }
        # rm.vec = as.numeric(rm.vec)
    }
    return(rm.vec)
}

#' @title make substitutions
#' @description make substitutions
#' @param dat a data frame
#' @importFrom stringr str_count
#'
makeSubstitutions <- function(dat) {
    # IMPORTANT: Have to sort the data frame just in case Have to sort by timestamp here, NOT by ab_num!
#    dat = dat[order(dat$timestamp), ]
    dat <- dplyr::arrange_(dat, ~timestamp)
    n = nrow(dat)
    top = which(dat$half == "top")
    bottom = which(dat$half != "top")
    
    # Pinch-hitters
    idx = which(dat$event %in% c("Offensive sub"))
    for (i in idx) {
        if (str_count(dat[i, "description"], "Pinch-hitter")) {
            x = which(dat$batterId == dat[i, "actionId"])
            # You can only pinch-hit once, and it must be the first time you appeared in the game
            if (length(x) > 0) {
                dat[x[1], "batterPos"] = "PH"
            }
        }
    }
    
    # Defensive substitutions
    idx = which(dat$event %in% c("Defensive Switch", "Defensive Sub", "Pitching Substitution"))
    for (i in idx) {
        if (dat[i, "half"] == "top") {
            off = bottom
            def = top
        } else {
            off = top
            def = bottom
        }
        if (str_count(dat[i, "description"], "Pitching Change:")) {
            dat[intersect(i:n, which(dat$batterId == dat[i, "actionId"])), "batterPos"] = "P"
        }
        if (str_count(dat[i, "description"], "(to designated hitter|as the designated hitter|playing designated hitter)")) {
            dat[intersect(i:n, which(dat$batterId == dat[i, "actionId"])), "batterPos"] = "DH"
        }
        if (str_count(dat[i, "description"], "(to catcher|as the catcher|playing catcher)")) {
            dat[intersect(i:n, def), "playerId.C"] = dat[i, "actionId"]
            dat[intersect(i:n, which(dat$batterId == dat[i, "actionId"])), "batterPos"] = "C"
        }
        if (str_count(dat[i, "description"], "(to first base|as the first baseman|playing first base)")) {
            dat[intersect(i:n, def), "playerId.1B"] = dat[i, "actionId"]
            dat[intersect(i:n, which(dat$batterId == dat[i, "actionId"])), "batterPos"] = "1B"
        }
        if (str_count(dat[i, "description"], "(to second base|as the second baseman|playing second base)")) {
            dat[intersect(i:n, def), "playerId.2B"] = dat[i, "actionId"]
            dat[intersect(i:n, which(dat$batterId == dat[i, "actionId"])), "batterPos"] = "2B"
        }
        if (str_count(dat[i, "description"], "(to third base|as the third baseman|playing third base)")) {
            dat[intersect(i:n, def), "playerId.3B"] = dat[i, "actionId"]
            dat[intersect(i:n, which(dat$batterId == dat[i, "actionId"])), "batterPos"] = "3B"
        }
        if (str_count(dat[i, "description"], "(to shortstop|as the shortstop|playing shortstop)")) {
            dat[intersect(i:n, def), "playerId.SS"] = dat[i, "actionId"]
            dat[intersect(i:n, which(dat$batterId == dat[i, "actionId"])), "batterPos"] = "SS"
        }
        if (str_count(dat[i, "description"], "(to left field|as the left fielder|playing left field)")) {
            dat[intersect(i:n, def), "playerId.LF"] = dat[i, "actionId"]
            dat[intersect(i:n, which(dat$batterId == dat[i, "actionId"])), "batterPos"] = "LF"
        }
        if (str_count(dat[i, "description"], "(to center field|as the center fielder|playing center field)")) {
            dat[intersect(i:n, def), "playerId.CF"] = dat[i, "actionId"]
            dat[intersect(i:n, which(dat$batterId == dat[i, "actionId"])), "batterPos"] = "CF"
        }
        if (str_count(dat[i, "description"], "(to right field|as the right fielder|playing right field)")) {
            dat[intersect(i:n, def), "playerId.RF"] = dat[i, "actionId"]
            dat[intersect(i:n, which(dat$batterId == dat[i, "actionId"])), "batterPos"] = "RF"
        }
    }
    # double-check to make sure you always have 9 distinct defenders X = data[!is.na(data$batterId), c('playerId.C',
    # 'playerId.1B', 'playerId.2B', 'playerId.3B', 'playerId.SS', 'playerId.LF', 'playerId.CF', 'playerId.RF')] sum(apply(X, 1,
    # duplicated))
    
    return(dat)
}

#' @title getFilederId
#' @description getFilderId
#' @param dat a \code{gameday} object
#' @importFrom stringr str_count

getFielderId = function(dat) {
    
    # Figure out who fielded the ball Flyouts
    fielderId <- with(dat, ifelse(event %in% c("Flyout", "Lineout", "Pop Out", "Bunt Pop Out") & str_count(description, " (flies|lines|pops) out( sharply| softly)? to pitcher "), 
        pitcherId, NA))
    fielderId <- with(dat, ifelse(event %in% c("Flyout", "Lineout", "Pop Out", "Bunt Pop Out") & str_count(description, " (flies|lines|pops) out( sharply| softly)? to catcher "), 
        playerId.C, fielderId))
    fielderId <- with(dat, ifelse(event %in% c("Flyout", "Lineout", "Pop Out", "Bunt Pop Out") & str_count(description, " (flies|lines|pops) out( sharply| softly)? to first baseman "), 
        playerId.1B, fielderId))
    fielderId <- with(dat, ifelse(event %in% c("Flyout", "Lineout", "Pop Out", "Bunt Pop Out") & str_count(description, " (flies|lines|pops) out( sharply| softly)? to second baseman "), 
        playerId.2B, fielderId))
    fielderId <- with(dat, ifelse(event %in% c("Flyout", "Lineout", "Pop Out", "Bunt Pop Out") & str_count(description, " (flies|lines|pops) out( sharply| softly)? to third baseman "), 
        playerId.3B, fielderId))
    fielderId <- with(dat, ifelse(event %in% c("Flyout", "Lineout", "Pop Out", "Bunt Pop Out") & str_count(description, " (flies|lines|pops) out( sharply| softly)? to shortstop "), 
        playerId.SS, fielderId))
    fielderId <- with(dat, ifelse(event %in% c("Flyout", "Lineout", "Pop Out", "Bunt Pop Out") & str_count(description, " (flies|lines|pops) out( sharply| softly)? to left fielder "), 
        playerId.LF, fielderId))
    fielderId <- with(dat, ifelse(event %in% c("Flyout", "Lineout", "Pop Out", "Bunt Pop Out") & str_count(description, " (flies|lines|pops) out( sharply| softly)? to center fielder "), 
        playerId.CF, fielderId))
    fielderId <- with(dat, ifelse(event %in% c("Flyout", "Lineout", "Pop Out", "Bunt Pop Out") & str_count(description, " (flies|lines|pops) out( sharply| softly)? to right fielder "), 
        playerId.RF, fielderId))
    # Groundouts with an assist
    fielderId <- with(dat, ifelse(event %in% c("Groundout", "Bunt Groundout") & str_count(description, " grounds out( sharply| softly)?(,| to) third baseman "), 
        playerId.3B, fielderId))
    fielderId <- with(dat, ifelse(event %in% c("Groundout", "Bunt Groundout") & str_count(description, " grounds out( sharply| softly)?(,| to) shortstop "), 
        playerId.SS, fielderId))
    fielderId <- with(dat, ifelse(event %in% c("Groundout", "Bunt Groundout") & str_count(description, " grounds out( sharply| softly)?(,| to) second baseman "), 
        playerId.2B, fielderId))
    fielderId <- with(dat, ifelse(event %in% c("Groundout", "Bunt Groundout") & str_count(description, " grounds out( sharply| softly)?(,| to) first baseman "), 
        playerId.1B, fielderId))
    fielderId <- with(dat, ifelse(event %in% c("Groundout", "Bunt Groundout") & str_count(description, " grounds out( sharply| softly)?(,| to) pitcher "), 
        pitcherId, fielderId))
    fielderId <- with(dat, ifelse(event %in% c("Groundout", "Bunt Groundout") & str_count(description, " grounds out( sharply| softly)?(,| to) catcher "), 
        playerId.C, fielderId))
    # Forceout
    fielderId <- with(dat, ifelse(event == "Forceout" & str_count(description, " (grounds|pops)( sharply| softly)? into a force out,( fielded by)? third baseman "), 
        playerId.3B, fielderId))
    fielderId <- with(dat, ifelse(event == "Forceout" & str_count(description, " (grounds|pops)( sharply| softly)? into a force out,( fielded by)? shortstop "), 
        playerId.SS, fielderId))
    fielderId <- with(dat, ifelse(event == "Forceout" & str_count(description, " (grounds|pops)( sharply| softly)? into a force out,( fielded by)? second baseman "), 
        playerId.2B, fielderId))
    fielderId <- with(dat, ifelse(event == "Forceout" & str_count(description, " (grounds|pops)( sharply| softly)? into a force out,( fielded by)? first baseman "), 
        playerId.1B, fielderId))
    fielderId <- with(dat, ifelse(event == "Forceout" & str_count(description, " (grounds|pops)( sharply| softly)? into a force out,( fielded by)? pitcher "), 
        pitcherId, fielderId))
    fielderId <- with(dat, ifelse(event == "Forceout" & str_count(description, " (grounds|pops)( sharply| softly)? into a force out,( fielded by)? catcher "), 
        playerId.C, fielderId))
    # Grounded Into DP
    fielderId <- with(dat, ifelse(event == "Grounded Into DP" & str_count(description, " grounds( sharply| softly)? into a double play, catcher "), 
        playerId.C, fielderId))
    fielderId <- with(dat, ifelse(event == "Grounded Into DP" & str_count(description, " grounds( sharply| softly)? into a double play, third baseman "), 
        playerId.3B, fielderId))
    fielderId <- with(dat, ifelse(event == "Grounded Into DP" & str_count(description, " grounds( sharply| softly)? into a double play, shortstop "), 
        playerId.SS, fielderId))
    fielderId <- with(dat, ifelse(event == "Grounded Into DP" & str_count(description, " grounds( sharply| softly)? into a double play, second baseman "), 
        playerId.2B, fielderId))
    fielderId <- with(dat, ifelse(event == "Grounded Into DP" & str_count(description, " grounds( sharply| softly)? into a double play, first baseman "), 
        playerId.1B, fielderId))
    fielderId <- with(dat, ifelse(event == "Grounded Into DP" & str_count(description, " grounds( sharply| softly)? into a double play, pitcher "), 
        pitcherId, fielderId))
    # Sac Bunts
    fielderId <- with(dat, ifelse(event %in% c("Sac Bunt", "Sac Fly", "Fielders Choice Out") & str_count(description, " on a (fielders choice out|sacrifice bunt|sacrifice fly)(,| to) pitcher "), 
        pitcherId, fielderId))
    fielderId <- with(dat, ifelse(event %in% c("Sac Bunt", "Sac Fly", "Fielders Choice Out") & str_count(description, " on a (fielders choice out|sacrifice bunt|sacrifice fly)(,| to) catcher "), 
        playerId.C, fielderId))
    fielderId <- with(dat, ifelse(event %in% c("Sac Bunt", "Sac Fly", "Fielders Choice Out") & str_count(description, " on a (fielders choice out|sacrifice bunt|sacrifice fly)(,| to) first baseman "), 
        playerId.1B, fielderId))
    fielderId <- with(dat, ifelse(event %in% c("Sac Bunt", "Sac Fly", "Fielders Choice Out") & str_count(description, " on a (fielders choice out|sacrifice bunt|sacrifice fly)(,| to) second baseman "), 
        playerId.2B, fielderId))
    fielderId <- with(dat, ifelse(event %in% c("Sac Bunt", "Sac Fly", "Fielders Choice Out") & str_count(description, " on a (fielders choice out|sacrifice bunt|sacrifice fly)(,| to) third baseman "), 
        playerId.3B, fielderId))
    fielderId <- with(dat, ifelse(event %in% c("Sac Bunt", "Sac Fly", "Fielders Choice Out") & str_count(description, " on a (fielders choice out|sacrifice bunt|sacrifice fly)(,| to) shortstop "), 
        playerId.SS, fielderId))
    fielderId <- with(dat, ifelse(event %in% c("Sac Bunt", "Sac Fly", "Fielders Choice Out") & str_count(description, " on a (fielders choice out|sacrifice bunt|sacrifice fly)(,| to) left fielder "), 
        playerId.LF, fielderId))
    fielderId <- with(dat, ifelse(event %in% c("Sac Bunt", "Sac Fly", "Fielders Choice Out") & str_count(description, " on a (fielders choice out|sacrifice bunt|sacrifice fly)(,| to) center fielder "), 
        playerId.CF, fielderId))
    fielderId <- with(dat, ifelse(event %in% c("Sac Bunt", "Sac Fly", "Fielders Choice Out") & str_count(description, " on a (fielders choice out|sacrifice bunt|sacrifice fly)(,| to) right fielder "), 
        playerId.RF, fielderId))
    
    return(fielderId)
}

recenter = function(dat, ...) {
    # From MLBAM specs
    dat = dplyr::mutate_(dat, our.x = ~x - 125)
    dat = dplyr::mutate_(dat, our.y = ~199 - y)
    # set distance from home to 2B
    scale = sqrt(90^2 + 90^2)/51
    dat = dplyr::mutate_(dat, r = ~scale * sqrt(our.x^2 + our.y^2))
    dat = dplyr::mutate_(dat, theta = ~atan2(our.y, our.x))
    dat = dplyr::mutate_(dat, our.x = ~r * cos(theta))
    dat = dplyr::mutate_(dat, our.y = ~r * sin(theta))
    return(dat)
}

#' @rdname plot.GameDayPlays
#' @export
#' @method plot gameday


plot.gameday = function(x, ...) { plot(x$ds, ...) }
    

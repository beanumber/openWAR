
  bat = read.csv("data/FanGraphs_2012_batting.csv")
  pitch = read.csv("data/FanGraphs_2012_pitching.csv")
  out = merge(x=bat, y=pitch, by = "playerid", all=TRUE)
  out$Name = with(out, ifelse(is.na(Name.x), as.character(Name.y), as.character(Name.x)))
  out$fRAA_bat = with(out, ifelse(is.na(Batting), 0, Batting) + ifelse(is.na(Positional), 0, Positional))
  out$fRAA_br = with(out, ifelse(is.na(Base.Running), 0, Base.Running))
  out$fRAA_field = with(out, ifelse(is.na(Fielding), 0, Fielding))
  out$fWAR_pitch = with(out, ifelse(is.na(WAR.y), 0, WAR.y))
  out$fRepl = with(out, ifelse(is.na(Replacement), 0, -Replacement))
  out$fRAR = with(out, ifelse(is.na(RAR.x), 0, RAR.x) + ifelse(is.na(RAR.y), 0, RAR.y))
  out$fRAA = with(out, fRAR + fRepl)
  out$fWAR = with(out, ifelse(is.na(WAR.x), 0, WAR.x) + fWAR_pitch)
  fWAR = out[, c("playerid", "Name", "fWAR", "fRAA_bat", "fRAA_br", "fRAA_field"
                 , "fWAR_pitch", "fRAR", "fRAA", "fRepl")]
  
  
save(fWAR, file="data/fWAR.rda")

  bat = read.csv("data/FanGraphs_2012_batting.csv")
  pitch = read.csv("data/FanGraphs_2012_pitching.csv")
  out = merge(x=bat, y=pitch, by = "playerid", all=TRUE)
  out$Name = with(out, ifelse(is.na(X.Name.x), as.character(X.Name.y), as.character(X.Name.x)))
  out$RAA_bat = with(out, ifelse(is.na(Batting), 0, Batting) + ifelse(is.na(Positional), 0, Positional))
  out$RAA_br = with(out, ifelse(is.na(Base.Running), 0, Base.Running))
  out$RAA_field = with(out, ifelse(is.na(Fielding), 0, Fielding))
  out$WAR_pitch = with(out, ifelse(is.na(WAR.y), 0, WAR.y))
  out$WAR = with(out, ifelse(is.na(WAR.x), 0, WAR.x) + WAR_pitch)
  fWAR = out[, c("playerid", "Name", "WAR", "RAA_bat", "RAA_br", "RAA_field", "WAR_pitch", "WAR")]
  
  
save(fWAR, file="data/fWAR.rda")
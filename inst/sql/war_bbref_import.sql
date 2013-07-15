
LOAD DATA LOCAL INFILE "../data/war_bbref_bat.txt" INTO TABLE war_bbref_batting FIELDS TERMINATED BY "," IGNORE 1 lines;
LOAD DATA LOCAL INFILE "../data/war_bbref_pitch.txt" INTO TABLE war_bbref_pitching FIELDS TERMINATED BY "," IGNORE 1 lines;
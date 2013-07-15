#


USE openwar;

--
-- Table structure for table `war_bbref_batting`
--

DROP TABLE IF EXISTS `war_bbref_batting`;
CREATE TABLE IF NOT EXISTS `war_bbref_batting` (
  `name_common` varchar(25) NOT NULL,
  `playerId` varchar(9) NOT NULL,
  `yearId` smallint(4) NOT NULL,
  `teamId` varchar(3) NOT NULL,
  `stintId` smallint(1) NOT NULL,
  `lgId` varchar(2) NOT NULL,
  `TPA` smallint(4) NOT NULL DEFAULT '0',
  `G` smallint(4) NOT NULL DEFAULT '0',
  `IP` decimal(5,1) NOT NULL DEFAULT '0.0',
  `R_BAT` decimal(5,1) NOT NULL DEFAULT '0.0',
  `R_BR` decimal(5,1) NOT NULL DEFAULT '0.0',
  `R_DP` decimal(5,1) NOT NULL DEFAULT '0.0',
  `R_FIELD` decimal(5,1) NOT NULL DEFAULT '0.0',
  `R_INF` decimal(5,1) NOT NULL DEFAULT '0.0',
  `R_OR` decimal(5,1) NOT NULL DEFAULT '0.0',
  `R_C` decimal(5,1) NOT NULL DEFAULT '0.0',
  `R_GOOD` decimal(5,1) NOT NULL DEFAULT '0.0',
  `R_DEF` decimal(5,1) NOT NULL DEFAULT '0.0',
  `R_POS` decimal(5,1) NOT NULL DEFAULT '0.0',
  `R_POS_P` decimal(5,1) NOT NULL DEFAULT '0.0',
  `R_REPL` decimal(5,1) NOT NULL DEFAULT '0.0',
  `R_ABOVE_REPL` decimal(5,1) NOT NULL DEFAULT '0.0',
  `R_ABOVE_AVG` decimal(5,1) NOT NULL DEFAULT '0.0',
  `R_ABOVE_AVG_OFF` decimal(5,1) NOT NULL DEFAULT '0.0',
  `R_ABOVE_AVG_DEF` decimal(5,1) NOT NULL DEFAULT '0.0',
  `WAA` decimal(5,1) NOT NULL DEFAULT '0.0',
  `WAA_OFF` decimal(5,1) NOT NULL DEFAULT '0.0',
  `WAA_DEF` decimal(5,1) NOT NULL DEFAULT '0.0',
  `WAR` decimal(5,1) NOT NULL DEFAULT '0.0',
  `WAR_DEF` decimal(5,1) NOT NULL DEFAULT '0.0',
  `WAR_OFF` decimal(5,1) NOT NULL DEFAULT '0.0',
  `WAR_REPL` decimal(5,1) NOT NULL DEFAULT '0.0',
  `salary` mediumint(8) NOT NULL DEFAULT '0',
  `isPitcher` char(1) NOT NULL DEFAULT 'N',
  `RPG_team` decimal(6,5) NOT NULL DEFAULT '0.00000',
  `RPG_opp` decimal(6,5) NOT NULL DEFAULT '0.00000',
  `RPPA_OPP_REP` decimal(6,5) NOT NULL DEFAULT '0.00000',
  `RPG_rep` decimal(6,5) NOT NULL DEFAULT '0.00000',
  `pyth_exp` decimal(6,5) NOT NULL DEFAULT '0.00000',
  `pyth_exp_rep` decimal(6,5) NOT NULL DEFAULT '0.00000',
  `waa_win_pct` decimal(6,5) NOT NULL DEFAULT '0.00000',
  `waa_win_pct_off` decimal(6,5) NOT NULL DEFAULT '0.00000',
  `waa_win_pct_def` decimal(6,5) NOT NULL DEFAULT '0.00000',
  `waa_win_pct_rep` decimal(6,5) NOT NULL DEFAULT '0.00000',
  PRIMARY KEY (`playerId`,`yearId`,`stintId`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;



DROP TABLE IF EXISTS `war_bbref_pitching`;
CREATE TABLE IF NOT EXISTS `war_bbref_pitching` (
  `name_common` varchar(25) NOT NULL,
  `playerId` varchar(9) NOT NULL,
  `yearId` smallint(4) NOT NULL,
  `teamId` varchar(3) NOT NULL,
  `stintId` smallint(1) NOT NULL,
  `lgId` varchar(2) NOT NULL,
  `G` smallint(4) NOT NULL DEFAULT '0',
  `GS` smallint(4) NOT NULL DEFAULT '0',
  `IPouts` smallint(4) NOT NULL DEFAULT '0',
  `IPouts_SP` smallint(4) NOT NULL DEFAULT '0',
  `IPouts_RP` smallint(4) NOT NULL DEFAULT '0',
  `RA` smallint(4) NOT NULL DEFAULT '0',
  `xRA` decimal(6,3) NOT NULL DEFAULT '0.000',
  `xRA_sprp_adj` decimal(6,3) NOT NULL DEFAULT '0.000',
  `xRA_def_pitcher` decimal(6,3) NOT NULL DEFAULT '0.000',
  `PPF` smallint(3) NOT NULL DEFAULT '0',
  `PPF_custom` decimal(6,3) NOT NULL DEFAULT '0.000',
  `xRA_final` decimal(6,3) NOT NULL DEFAULT '0.000',
  `BIP` smallint(4) NOT NULL DEFAULT '0',
  `BIP_PCT` decimal(5,4) NOT NULL DEFAULT '0.0000',
  `RS_DEF_total` decimal(4,1) NOT NULL DEFAULT '0.0',
  `RAA` decimal(6,3) NOT NULL DEFAULT '0.000',
  `RAA_adj` decimal(6,3) NOT NULL DEFAULT '0.000',
  `RAR` decimal(6,3) NOT NULL DEFAULT '0.000',
  `RPO_REPL` decimal(5,3) NOT NULL DEFAULT '0.000',
  `GR_LEVERAGE_INDEX_AVG` decimal(5,4) NOT NULL DEFAULT '0.0000',
  `WAR` decimal(5,1) NOT NULL DEFAULT '0.0',
  `salary` mediumint(8) NOT NULL DEFAULT '0',
  `RPG_team` decimal(6,5) NOT NULL DEFAULT '0.00000',
  `RPG_opp` decimal(6,5) NOT NULL DEFAULT '0.00000',
  `pyth_exp` decimal(4,3) NOT NULL DEFAULT '0.000',
  `waa_win_pct` decimal(6,5) NOT NULL DEFAULT '0.00000',
  `WAA` decimal(6,4) NOT NULL DEFAULT '0.0000',
  `WAA_adj` decimal(6,4) NOT NULL DEFAULT '0.0000',
  `RPPA_OPP_REP` decimal(6,5) NOT NULL DEFAULT '0.00000',
  `pyth_exp_rep` decimal(4,3) NOT NULL DEFAULT '0.000',
  `waa_win_pct_rep` decimal(5,4) NOT NULL DEFAULT '0.0000',
  `WAR_rep` decimal(6,4) NOT NULL DEFAULT '0.0000',
  PRIMARY KEY (`playerId`,`yearId`,`stintId`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;


DROP TABLE IF EXISTS `war_bbref`;
CREATE TABLE `war_bbref` AS
	SELECT b.playerId, b.yearId, b.stintId, b.teamId, b.lgId
	, R_BAT, R_BR, R_DP, R_FIELD, R_POS, R_REPL
	, b.R_ABOVE_AVG + ifnull(p.RAA, 0) as RAA
	, b.R_ABOVE_REPL +ifnull(p.RAR, 0) as RAR
	, b.WAR + ifnull(p.WAR, 0) as WAR
	FROM war_bbref_batting b 
	LEFT JOIN war_bbref_pitching p USING (playerId, yearId, stintId);
ALTER TABLE war_bbref ADD PRIMARY KEY (playerId, yearId, stintId);
INSERT IGNORE INTO `war_bbref` 
	SELECT p.playerId, p.yearId, p.stintId, p.teamId, p.lgId
	, R_BAT, R_BR, R_DP, R_FIELD, R_POS, R_REPL
	, ifnull(b.R_ABOVE_AVG,0) + p.RAA as RAA
	, ifnull(b.R_ABOVE_REPL,0) + p.RAR as RAR
	, ifnull(b.WAR,0) + p.WAR as WAR
	FROM war_bbref_pitching p 
	LEFT JOIN war_bbref_batting b USING (playerId, yearId, stintId);







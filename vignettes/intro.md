<!--
%\VignetteEngine{knitr::knitr}
%\VignetteIndexEntry{Introduction to openWAR}
-->


### Data Acquisition

We have developed an R package to compute our implementation of WAR. The first step in that process is to get meaningful play-by-play data. We have written parsers that will download and extract data from the Major League Baseball Advanced Media (MLBAM) GameDay server. This data is updated live, and available to the public. Thus, although this data is not "free as in freedom", it is "free as in beer." 

Using our **openWAR** package, a single game's worth of play-by-play data can be retrieved from the GameDay servers and processed into a data frame. The R class **gameday** contains an object that includes the relevant URLs and XML files, as well as the processed data. The XML files and processed data are cached locally for faster retrieval. Note that since the MLBAM data is not transferrable, all data must be downloaded by the user at least once. 


```r
require(openWAR)
```

```
## Warning: replacing previous import by 'mosaic::do' when loading 'openWAR'
```

```
## Warning: replacing previous import by 'mosaic::tally' when loading
## 'openWAR'
```

```
## Warning: replacing previous import by 'mosaic::count' when loading
## 'openWAR'
```

```
## Warning: replacing previous import by 'stringr::%>%' when loading 'openWAR'
```

```
## Warning: replacing previous import by 'dplyr::do' when loading 'openWAR'
```

```
## Warning: replacing previous import by 'dplyr::group_by' when loading
## 'openWAR'
```

```
## Warning: replacing previous import by 'dplyr::mutate' when loading
## 'openWAR'
```

```
## Warning: replacing previous import by 'dplyr::summarize' when loading
## 'openWAR'
```

```
## Warning: replacing previous import by 'stringr::str_count' when loading
## 'openWAR'
```

```
## Warning: replacing previous import by 'stringr::str_split' when loading
## 'openWAR'
```

```r
gd = gameday()
# Equivalently
data(MetsBraves)
```

#### Single Game data

The default game was played on August 12th, 2012 between the New York Mets and the Atlanta Braves. 


```r
gd$gameId
```

```
## [1] "gid_2012_08_12_atlmlb_nynmlb_1"
```

The directory on the GameDay server that contains that actual XML files is located here.


```r
gd$base
```

```
## [1] "http://gd2.mlb.com/components/game/mlb/year_2012/month_08/day_12/"
```

In this game, the Braves beat the Mets, 6-5.


```r
summary(gd)
```

```
##        Length Class      Mode     
## gameId  1     -none-     character
## base    1     -none-     character
## url     5     -none-     character
## ds     62     data.frame list
```

Our primary interest will be in analyzing the play-by-play data that we have processed for this game. This data contains a complete record of what happened in the game. For example, this game started with Michael Bourn leading off with a double. After a walk to Martin Prado and a strikeout of Jason Heyward, Chipper Jones grounded into an inning-ending 5-4-3 double play. 


```r
head(gd$ds)
```

```
##   pitcherId batterId field_teamId ab_num inning   half balls strikes
## 1    477003   456422          121      1      1    top     0       1
## 2    477003   445988          121      2      1    top     4       0
## 3    477003   518792          121      3      1    top     1       3
## 4    477003   116706          121      4      1    top     1       2
## 5    282656   514913          144      5      1 bottom     2       2
## 6    282656   488689          144      6      1 bottom     1       1
##   endOuts            event actionId
## 1       0           Double       NA
## 2       0             Walk       NA
## 3       1        Strikeout       NA
## 4       3 Grounded Into DP       NA
## 5       1           Flyout       NA
## 6       1           Single       NA
##                                                                                                                                                     description
## 1                                                                               Michael Bourn doubles (21) on a line drive to left fielder Jordany Valdespin.  
## 2                                                                                                                                         Martin Prado walks.  
## 3                                                                                                                         Jason Heyward strikes out swinging.  
## 4 Chipper Jones grounds into a double play, third baseman David Wright to second baseman Daniel Murphy to first baseman Ike Davis.   Martin Prado out at 2nd.  
## 5                                                                                                     Ruben Tejada flies out to center fielder Michael Bourn.  
## 6                                                                                         Mike Baxter singles on a line drive to right fielder Jason Heyward.  
##   stand throws
## 1     L      L
## 2     R      L
## 3     L      L
## 4     R      L
## 5     R      R
## 6     L      R
##                                                                               runnerMovement
## 1                                                                       [456422::2B::Double]
## 2                                                                         [445988::1B::Walk]
## 3                                                                                           
## 4 [456422:2B:3B::Stolen Base 3B][445988:1B:::Grounded Into DP][456422:3B:::Grounded Into DP]
## 5                                                                                           
## 6                                                                       [488689::1B::Single]
##        x      y game_type home_team home_teamId home_lg away_team
## 1  61.24 120.48         R       nyn         121      NL       atl
## 2     NA     NA         R       nyn         121      NL       atl
## 3     NA     NA         R       nyn         121      NL       atl
## 4     NA     NA         R       nyn         121      NL       atl
## 5 111.45  80.32         R       nyn         121      NL       atl
## 6 158.63 106.43         R       nyn         121      NL       atl
##   away_teamId away_lg venueId    stadium           timestamp playerId.C
## 1         144      NL    3289 Citi Field 2012-08-13 00:06:39     453531
## 2         144      NL    3289 Citi Field 2012-08-13 00:07:43     453531
## 3         144      NL    3289 Citi Field 2012-08-13 00:09:29     453531
## 4         144      NL    3289 Citi Field 2012-08-13 00:11:16     453531
## 5         144      NL    3289 Citi Field 2012-08-13 00:15:11     435263
## 6         144      NL    3289 Citi Field 2012-08-13 00:17:48     435263
##   playerId.1B playerId.2B playerId.3B playerId.SS playerId.LF playerId.CF
## 1      477195      502517      431151      514913      518170      400083
## 2      477195      502517      431151      514913      518170      400083
## 3      477195      502517      431151      514913      518170      400083
## 4      477195      502517      431151      514913      518170      400083
## 5      518692      462564      116706      457926      445988      456422
## 6      518692      462564      116706      457926      445988      456422
##   playerId.RF batterPos batterName pitcherName runsOnPlay startOuts
## 1      488689        CF      Bourn       Niese          0         0
## 2      488689        LF      Prado       Niese          0         0
## 3      488689        RF    Heyward       Niese          0         0
## 4      488689        3B   Jones, C       Niese          0         1
## 5      518792        SS  Tejada, R      Sheets          0         0
## 6      518792        RF     Baxter      Sheets          0         1
##   runsInInning runsITD runsFuture start1B start2B start3B  end1B  end2B
## 1            0       0          0    <NA>    <NA>    <NA>   <NA> 456422
## 2            0       0          0    <NA>  456422    <NA> 445988 456422
## 3            0       0          0  445988  456422    <NA> 445988 456422
## 4            0       0          0  445988  456422    <NA>   <NA>   <NA>
## 5            2       0          2    <NA>    <NA>    <NA>   <NA>   <NA>
## 6            2       0          2    <NA>    <NA>    <NA> 488689   <NA>
##   end3B outsInInning startCode endCode fielderId
## 1  <NA>            3         0       2        NA
## 2  <NA>            3         2       3        NA
## 3  <NA>            3         3       3        NA
## 4  <NA>            3         3       0    431151
## 5  <NA>            3         0       0    456422
## 6  <NA>            3         0       1        NA
##                           gameId isPA  isAB isHit isBIP      our.x
## 1 gid_2012_08_12_atlmlb_nynmlb_1 TRUE  TRUE  TRUE  TRUE -159.12398
## 2 gid_2012_08_12_atlmlb_nynmlb_1 TRUE FALSE FALSE FALSE         NA
## 3 gid_2012_08_12_atlmlb_nynmlb_1 TRUE  TRUE FALSE FALSE         NA
## 4 gid_2012_08_12_atlmlb_nynmlb_1 TRUE  TRUE FALSE FALSE         NA
## 5 gid_2012_08_12_atlmlb_nynmlb_1 TRUE  TRUE FALSE  TRUE  -33.81634
## 6 gid_2012_08_12_atlmlb_nynmlb_1 TRUE  TRUE  TRUE  TRUE   83.92942
##      our.y        r    theta
## 1 195.9601 252.4298 2.252825
## 2       NA       NA       NA
## 3       NA       NA       NA
## 4       NA       NA       NA
## 5 296.1862 298.1104 1.684477
## 6 231.0243 245.7974 1.222329
```

#### Many games

More often, we'll be interested in investigated data from many games. The function **getData()** will load (or download) data over any time interval in which you are interested. Let's figure out how many home runs were hit on May 14th, 2013. 


```r
ds = getData(start = "2013-05-14")
```

```
## 
## Retrieving data from 2013-05-14 ...
## ...found 15 games
```

```r
subset(ds, event == "Home Run", select = c("gameId", "batterId", "description"))
```

```
##                              gameId batterId
## 70   gid_2013_05_14_bosmlb_tbamlb_1   120074
## 144  gid_2013_05_14_chamlb_minmlb_1   276055
## 145  gid_2013_05_14_chamlb_minmlb_1   493364
## 286  gid_2013_05_14_clemlb_phimlb_1   435623
## 330  gid_2013_05_14_clemlb_phimlb_1   502126
## 369  gid_2013_05_14_colmlb_chnmlb_1   458913
## 374  gid_2013_05_14_colmlb_chnmlb_1   471865
## 398  gid_2013_05_14_colmlb_chnmlb_1   446381
## 427  gid_2013_05_14_colmlb_chnmlb_1   471865
## 493  gid_2013_05_14_houmlb_detmlb_1   408234
## 533  gid_2013_05_14_kcamlb_anamlb_1   405395
## 536  gid_2013_05_14_kcamlb_anamlb_1   435062
## 549  gid_2013_05_14_kcamlb_anamlb_1   456714
## 552  gid_2013_05_14_kcamlb_anamlb_1   285078
## 562  gid_2013_05_14_kcamlb_anamlb_1   545361
## 579  gid_2013_05_14_milmlb_pitmlb_1   516416
## 674  gid_2013_05_14_milmlb_pitmlb_1   457705
## 721  gid_2013_05_14_nynmlb_slnmlb_1   136860
## 729  gid_2013_05_14_nynmlb_slnmlb_1   407781
## 732  gid_2013_05_14_nynmlb_slnmlb_1   445055
## 759  gid_2013_05_14_sdnmlb_balmlb_1   435041
## 773  gid_2013_05_14_sdnmlb_balmlb_1   475247
## 865  gid_2013_05_14_seamlb_nyamlb_1   116380
## 930  gid_2013_05_14_sfnmlb_tormlb_1   474832
## 980  gid_2013_05_14_sfnmlb_tormlb_1   467055
## 1007 gid_2013_05_14_texmlb_oakmlb_1   519048
## 1060 gid_2013_05_14_texmlb_oakmlb_1   134181
## 1062 gid_2013_05_14_texmlb_oakmlb_1   519048
##                                                                                                           description
## 70      David Ortiz homers (5) on a line drive to right field.    Jacoby Ellsbury scores.    Dustin Pedroia scores.  
## 144                                                        Adam Dunn homers (7) on a fly ball to left center field.  
## 145                                                           Dayan Viciedo homers (3) on a fly ball to left field.  
## 286                                                          Kevin Frandsen homers (2) on a fly ball to left field.  
## 330                                                        Domonic Brown homers (7) on a line drive to right field.  
## 369                              Eric Young Jr.   homers (1) on a fly ball to center field.   Josh Rutledge scores.  
## 374                                                      Carlos Gonzalez homers (8) on a line drive to right field.  
## 398                                                           Darwin Barney homers (2) on a fly ball to left field.  
## 427                                                        Carlos Gonzalez homers (9) on a fly ball to right field.  
## 493                                                          Miguel Cabrera homers (8) on a fly ball to left field.  
## 533                                                           Albert Pujols homers (6) on a fly ball to left field.  
## 536                                                        Howie Kendrick homers (6) on a fly ball to center field.  
## 549                                                    Billy Butler homers (5) on a fly ball to right center field.  
## 552                                                         Josh Hamilton homers (5) on a fly ball to center field.  
## 562                                                       Mike Trout homers (7) on a fly ball to left center field.  
## 579                                                             Jean Segura homers (7) on a fly ball to left field.  
## 674                                                      Andrew McCutchen homers (5) on a fly ball to center field.  
## 721               Carlos Beltran homers (10) on a fly ball to left field.   Pete Kozma scores.    John Gast scores.  
## 729                                  Marlon Byrd homers (3) on a fly ball to left center field.   John Buck scores.  
## 732                                                               Jon Jay homers (4) on a fly ball to center field.  
## 759                                                          Carlos Quentin homers (4) on a fly ball to left field.  
## 773                                                         Ryan Flaherty homers (2) on a line drive to left field.  
## 865                                Raul Ibanez homers (4) on a line drive to right field.    Kelly Shoppach scores.  
## 930                                                    Brandon Belt homers (5) on a fly ball to right center field.  
## 980  Pablo Sandoval homers (7) on a fly ball to left center field.   Andres Torres scores.    Marco Scutaro scores.  
## 1007                        Mitch Moreland homers (8) on a fly ball to right center field.    Adrian Beltre scores.  
## 1060                                                   Adrian Beltre homers (9) on a fly ball to left center field.  
## 1062                                                       Mitch Moreland homers (9) on a fly ball to center field.
```

#### Visualizing the data

The best part about the MLBAM data is that it contains an $(x,y)$-coordinate indicated the location of each batted ball hit into play. We can visualize this. 


```r
plot(data = ds)
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

### Modeling

In order to compute **openWAR**, we need to model several quantities. The first thing we need to understand is the relative value of each "state" of a half-inning. Since there are three bases, each of which can be either occupied or unoccupied, and there are three possible numbers of outs, each plate appearance begins with the half-inning in one of 25 possible states (the 24 states, plus one last state for three outs). We would like to assign a value to each one of these states that indicates the expected number of runs that will be scored in the remainder of that half-inning. We have precomputed the states and the number of **futureRuns** associated with each play. 

Thus, we want to fit the model
$$
  futureRuns \sim baseCode + outs + baseCode \cdot outs,
$$
where $baseCode$ is a description of the configuration of the baserunners, and $outs$ is the number of outs in the half-inning. 

For example, consider the bottom of the 1st inning of our game:


```r
subset(gd$ds, inning == 1 & half == "bottom", select=c("runsFuture", "runsOnPlay", "startCode", "startOuts", "description"))
```

```
##    runsFuture runsOnPlay startCode startOuts
## 5           2          0         0         0
## 6           2          0         0         1
## 7           2          1         1         1
## 8           1          1         4         1
## 9           0          0         1         1
## 10          0          0         1         2
##                                                                                                                                                                             description
## 5                                                                                                                             Ruben Tejada flies out to center fielder Michael Bourn.  
## 6                                                                                                                 Mike Baxter singles on a line drive to right fielder Jason Heyward.  
## 7  David Wright doubles (34) on a line drive to right fielder Jason Heyward.   Mike Baxter scores.  David Wright advances to 3rd, on a throwing error by right fielder Jason Heyward.  
## 8                                                                                            Ike Davis singles on a line drive to right fielder Jason Heyward.   David Wright scores.  
## 9                                                                                                                               Daniel Murphy lines out to left fielder Martin Prado.  
## 10                                                                                                                                       Jordany Valdespin strikes out on a foul tip.
```

The Mets scored two runs in the inning, and thus, when Ruben Tejada opened the inning, there were no runners on base, no outs, but two $futureRuns$ were associated with this play. After Tejada flew out, there was one out, but still no one on base and two $futureRuns$. After Mike Baxter singles, David Wright came to the plate with a runner on first (bc_before = 1), one out, and two $futureRuns$. His double scored one run, so Ike Davis followed with a runner on third, one out, and now only one $futureRuns$. By the time Daniel Murphy bats, there are no further $futureRuns$ in the inning. 

Every inning begins with no one on and no one out. In this example, two runs scored in the inning. By averaging over all innings, we create an estimate of the expected $futureRuns$ for the state $(0,0)$. But we can just as easily do the same for all states. 

#### Building a model for expected runs

The simplest way to build a model for $futureRuns$ is to take the average over all observations. To do this, we'll need more data. 


```r
# Will take a loooong time -- the first time
# ds = getDataWeekly("2013-04-01")
# ds = getDataWeekly("2013-04-08")
# ds = getDataWeekly("2013-04-15")
# ds = getDataWeekly("2013-04-22")
# ds = getData("2013-03-31")
# 2013 first half
# ds = getData("2013-03-31", end="2013-07-14")

# ds = getDataMonthly(2013, 6)
# MLBAM2013 = ds
# save(MLBAM2013, file="data/MLBAM2013.rda")
data(MLBAM2013)
ds = MLBAM2013
```


For example, consider the half inning we visited previously. 


```r
subset(gd$ds, inning == 1 & half == "bottom", select=c("runsFuture", "runsOnPlay", "startCode", "startOuts", "description"))
```

```
##    runsFuture runsOnPlay startCode startOuts
## 5           2          0         0         0
## 6           2          0         0         1
## 7           2          1         1         1
## 8           1          1         4         1
## 9           0          0         1         1
## 10          0          0         1         2
##                                                                                                                                                                             description
## 5                                                                                                                             Ruben Tejada flies out to center fielder Michael Bourn.  
## 6                                                                                                                 Mike Baxter singles on a line drive to right fielder Jason Heyward.  
## 7  David Wright doubles (34) on a line drive to right fielder Jason Heyward.   Mike Baxter scores.  David Wright advances to 3rd, on a throwing error by right fielder Jason Heyward.  
## 8                                                                                            Ike Davis singles on a line drive to right fielder Jason Heyward.   David Wright scores.  
## 9                                                                                                                               Daniel Murphy lines out to left fielder Martin Prado.  
## 10                                                                                                                                       Jordany Valdespin strikes out on a foul tip.
```

The inning began in the state $(0,0)$. Our estimate $\hat{\rho}(0,0)$ of the expected value (in runs) of that state is:


```r
fit.rem = getRunEx(ds)
fit.rem(baseCode = 0, outs = 0)
```

```
## [1] 0.4565559
```

```r
# Note this is equivalent to 
# rem[1,1]
```

On the first play of the inning, Ruben Tejada flied out. This moved the inning into the state $(0,1)$, since there were still no runners on base, but now there was one out. The value of this state is 


```r
fit.rem(0,1)
```

```
## [1] 0.2359775
```

The difference between these two states is $\hat{\delta}_i$:


```r
fit.rem(0,1) - fit.rem(0,0)
```

```
## [1] -0.2205784
```

In modeling this play, our goal is to apportion the value of $\hat{\delta}_i$ to each of the offensive players. In this case, Tejada was the only offensive player involved, so he gets the full amount. Moreover, $-\hat{\delta}_i$ must also be attributed to the defense. In this case, some of that credit will go to the pitcher, and some will go to the centerfielder. The details of this apportionment scheme will be revealed later. 

The second batter, Mike Baxter, singled. This moved the inning from $(0,1)$ to $(1, 1)$. Accordingly, Baxter would receive:


```r
fit.rem(1,1) - fit.rem(0,1)
```

```
## [1] 0.287121
```

So far, so good. The next play is particularly complicated. David Wright doubles homes Baxter, and then advances to third on a throwing error by the rightfielder. Let's assume for a moment that the error didn't happen, and that Wright end the play on second base. In this case, the ending state is $(2,1)$, but in addition, one run scored. Thus, the change in expected runs is:


```r
fit.rem(2,1) - fit.rem(1,1) + 1
```

```
## [1] 1.115999
```

Clearly, much of the credit here should go to Wright, for hitting the double. But what about Baxter, who scored from first on a double? Our plan is to assume "ghostrunner" rules, wherein the number of bases advanced by each baserunner is determined by the type of hit. Since Wright hit a double, Baxter should have advanced two bases, leaving the inning in the state $(6,1)$. The additional base that he advanced (from third to home) should then be given to Baxter. Thus, as a batter, Wright accrues:


```r
fit.rem(6,1) - fit.rem(1,1)
```

```
## [1] 0.7966985
```

While Baxter accrues the remainder:


```r
fit.rem(2,1) - fit.rem(6,1) + 1
```

```
## [1] 0.3193008
```

But now let's revisit what actually happened. Heyward's error allowed Wright to move to third. Thus, the state before the error occurred was $(2,1)$ and it led to $(4,1)$. The difference


```r
fit.rem(4,1) - fit.rem(2,1)
```

```
## [1] 0.2706767
```

goes to Heyward as a rightfielder, and Wright as a baserunner. 

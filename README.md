OpenWAR
================

[![Travis-CI Build Status](https://travis-ci.org/beanumber/openWAR.svg?branch=master)](https://travis-ci.org/beanumber/openWAR)

### An open-source system for computing Wins Above Replacement

This package is designed to present a reference implementation of [Wins Above Replacement](http://en.wikipedia.org/wiki/Wins_above_replacement) for Major League Baseball players.

#### Installation

The **Sxslt** package is required in order to download new game data from MLBAM. This package is not present on CRAN. Hence, some manual installation may be necessary. The following command:

``` r
devtools::install_github("omegahat/Sxslt")
```

should do the trick. If it doesn't please check that your operating system meets the `SystemRequirements`. These can be installed on Ubuntu by:

``` bash
sudo apt-get install libxslt1-dev libxslt1.1 libxml2 libxml2-dev
```

Next, installing **openWAR** is best accomplished through the *install\_github* function in the **devtools** package.

``` r
devtools::install_github("beanumber/openWAR")
```

#### Data Source

The *gameday* function downloads play-by-play data from the GameDay server hosted by Major League Baseball Advanced Media. This data is not *libre*, but it lives on a publicly-available webserver.

Getting individual game data is as simple as:

``` r
library(openWAR)
gd <- gameday()
```

    ## gid_2012_08_12_atlmlb_nynmlb_1

``` r
summary(gd)
```

    ##        Length Class        Mode     
    ## gameId  1     -none-       character
    ## base    1     -none-       character
    ## url     5     -none-       character
    ## ds     62     GameDayPlays list

``` r
plot(gd)
```

![](README_files/figure-markdown_github/unnamed-chunk-4-1.png)

To retrieve a `data.frame` of many games worth, try:

``` r
ds <- getData()
```

This will retrieve play-by-play data for all games played yesterday (by default). For each play, 62 variables are recorded.

#### Methodology

Please see our full paper on the [arXiv](http://arxiv.org/abs/1312.7158) or in [*Journal of Quantitative Analysis in Sports*](https://www.degruyter.com/view/j/jqas.2015.11.issue-2/jqas-2014-0098/jqas-2014-0098.xml).

## OpenWAR
### An open-source system for computing Wins Above Replacement

This package is designed to present a reference implementation of [Wins Above Replacement](http://en.wikipedia.org/wiki/Wins_above_replacement) for Major League Baseball players. 

#### Installation

The **Sxslt** package is required in order to download new game data from MLBAM. This package is not present on CRAN. Hence, some manual installation may be necessary. The following command:

```{r}
install.packages("Sxslt", repos = "http://www.omegahat.org/R", type = "source")
```
should do the trick. 

Next, installing **openWAR** is best accomplished through the *install_github()* function in the **devtools** package. 

```{r}
require(devtools)
install_github("beanumber/openWAR")
```

#### Data Source

The *gameday()* function downloads play-by-play data from the GameDay server hosted by Major League Baseball Advanced Media. This data is not *libre*, but it lives on a publicly-available webserver. 

Getting individual game data is as simple as:

```{r}
require(openWAR)
gd = gameday()
summary(gd)
```

To retrieve a data.frame of many games worth, try:

```{r}
ds = getData()
```

This will retrieve play-by-play data for all games played yesterday (by default). For each play, 62 variables are recorded. 

#### Methodology


Predicting the Bachelor Season 21
================

The goal of this analysis is to build a model that will accurately predict the winner of the Bachelor season 21. The data in this analysis comes from the Wikipedia pages for each season of the Bachelor.
*Note: There are no Wikipedia Pages for Seasons 3,4,6,7,or 8 and therefore, they seasons are excluded from this analysis*

Packages Needed
---------------

``` r
library(dplyr)
library(htmltab)
```

Scraping Data
-------------

### Scraping Contestant Info - Past Seasons

``` r
# Extract Season 2 First (Different Page Format)
url <- "https://en.wikipedia.org/wiki/The_Bachelor_(season_2)"
contestant <- htmltab(doc=url, which='//*[@id="mw-content-text"]/table[3]')
contestant$season <- 2

# Build Function to Scrape other Seasons
scrape_bach_season <- function(season){
  url <- paste("https://en.wikipedia.org/wiki/The_Bachelor_(season_",season,")",sep="")
  sn <- htmltab(doc=url, which='//*[@id="mw-content-text"]/table[2]', rm_nodata_cols = F)

  names(sn)[names(sn)=="Outcome"] <- "Eliminated"
  names(sn)[names(sn)=="Occupation"] <- "Job"
  sn <- select(sn, Name,Age,Hometown,Job,Eliminated)
  sn$season <- season
  
  return(sn)
}

# ID Valid Seasons
valid_seasons <- c(1,5,9:20)

# Scrape Valid Seasons (Ones with Wikipedia Pages)
for(i in valid_seasons){
  sn <- scrape_bach_season(i)
  contestant <- rbind(contestant,sn)
}
```

### Scraping Contestant Info - Current Seasons

``` r
url <- "https://en.wikipedia.org/wiki/The_Bachelor_(season_21)"
contestant21 <- htmltab(doc=url, which='//*[@id="mw-content-text"]/table[2]')
names(contestant21)[names(contestant21)=="Outcome"] <- "Eliminated"
names(contestant21)[names(contestant21)=="Occupation"] <- "Job"
```

### Scraping Bachelor Info

``` r
url <- "https://en.wikipedia.org/wiki/The_Bachelor_(U.S._TV_series)"
bachelor <- htmltab(doc=url, which='//*[@id="mw-content-text"]/table[2]')
names(bachelor)[names(bachelor)=="Occupation"] <- "Job"

bachelor <- filter(bachelor, Season %in% c(1,2,5,9:21))

# Manually Pull Birth Date and Hometown
bdate <- c("1970-08-10", "1974-04-22", "1978-10-05",
           "1972-06-09", "1977-02-05", "1972-11-10",
           "1980-07-17", "1976-07-05", "1978-01-27",
           "1972-11-10", "1982-09-10", "1983-11-16",
           "1981-08-05", "1981-11-06", "1988-03-23",
           "1980-09-29")
Hometown <- c("Charolottesville, Virginia", "Butler, Missouri", "Toronto, Ontario",
              "Milan, Italy","Lancaster, Pennsylvania", "Atlanta, Georgia",
              "London, England", "Cleveland, Ohio", "Dallas, Texas",
              "Atlanta, Georgia", "Sonoma, California", "Irving, Texas",
              "Barquisimeto, Venezuela", "Arlington, Iowa", "Denver, Colorado",
              "Milwaukee, Wisconsin")

bachelor$bdate <- bdate
bachelor$Hometown <- Hometown
```

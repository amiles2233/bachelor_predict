setwd("~/Dropbox/R/Bachelor Prediction/bachelor_predict")

library(dplyr)
library(htmltab)

# Pull Contestant Info, Previous Seasons -------------------
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

contestant <- rename(contestant, Occupation=Job)


# Pull Contestant Info, Current Season -----------------
url <- "https://en.wikipedia.org/wiki/The_Bachelor_(season_21)"
contestant21 <- htmltab(doc=url, which='//*[@id="mw-content-text"]/table[2]')
names(contestant21)[names(contestant21)=="Outcome"] <- "Eliminated"
names(contestant21)[names(contestant21)=="Occupation"] <- "Job"

contestant21 <- rename(contestant21, Occupation=Job) %>%
  select(-Place) %>%
  mutate(season=21)
  

# Combine All Contestants
contestant <- bind_rows(contestant, contestant21)

# Lowercase names
names(contestant) <- tolower(names(contestant))

# Write Contestant CSV
write.csv(contestant,"contestant.csv", row.names = FALSE)


# Pull Bachelor Info, All Seasons --------------
url <- "https://en.wikipedia.org/wiki/The_Bachelor_(U.S._TV_series)"
bachelor <- htmltab(doc=url, which='//*[@id="mw-content-text"]/table[2]')

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

names(bachelor) <- tolower(names(bachelor))

# Save Results
write.csv(bachelor,"bachelor.csv", row.names=FALSE)





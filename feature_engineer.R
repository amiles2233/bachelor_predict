library(plyr)
library(dplyr)
library(caret)
library(ggmap)
library(geosphere)
library(lubridate)
library(tidytext)

# Read Data---------------
contestant <- read.csv("contestant.csv", stringsAsFactors = FALSE)
bachelor <- read.csv("bachelor.csv", stringsAsFactors = FALSE)

# Rename New York, New York to New York City, New York (geocode misses otherwise)
contestant$hometown[contestant$hometown=="New York, New York"] <- "New York City, New York"

# Geocode Hometowns---------------
contest_loc <- geocode(contestant$hometown)
bach_loc <- geocode(bachelor$hometown)

# ID Contestant Clusters
set.seed(8675309)
km7 <- kmeans(contest_loc, centers = 7)
contest_loc$region <- factor(km7$cluster)
dv <- dummyVars( ~ region, data=contest_loc)
clustDum <- data.frame(predict(dv, newdata=contest_loc))

contestant <- bind_cols(contestant,contest_loc) %>%
  bind_cols(clustDum)
bachelor <- bind_cols(bachelor, bach_loc)

## Save so I don't have to do geocoding again
saveRDS(contestant, "contestant.RDS")
saveRDS(bachelor, "bachelor.RDS")



# Occupational Dummies ------------
contestant_occ <- contestant %>%
  select(name, occupation) %>%
  unnest_tokens(word, occupation) %>%
  count(word, sort=TRUE)
contestant$occupation <- tolower(contestant$occupation)
contestant$manager <- ifelse(grepl("manager", contestant$occupation),1,0)
contestant$teacher <- ifelse(grepl("teacher", contestant$occupation),1,0)
contestant$nurse <- ifelse(grepl("nurse", contestant$occupation),1,0)
contestant$student <- ifelse(grepl("student", contestant$occupation),1,0)
contestant$sales <- ifelse(grepl("sales", contestant$occupation),1,0)
contestant$attorney <- ifelse(grepl("attorney", contestant$occupation),1,0)
contestant$model <- ifelse(grepl("model", contestant$occupation),1,0)
contestant$manager <- ifelse(grepl("manager", contestant$occupation),1,0)
contestant$real_estate <- ifelse(grepl("real estate", contestant$occupation),1,0)
contestant$owner <- ifelse(grepl("owner", contestant$occupation),1,0)


# Calculate Bachelor Age
# Format Season Start Date
bachelor$bdate <- ymd(bachelor$bdate)
bachelor$season_start <- mdy(sub("–.*,", "", bachelor$original.run))
bachelor$age <- as.numeric(difftime(bachelor$season_start, bachelor$bdate, units="days"))/365.25


# Keep only relevant bachelor vars before joinin
bachelor <- select(bachelor, season, bachelor, winner, lon, lat, age) %>%
  rename(lon_bach=lon, lat_bach=lat, age_bach=age)


# Join Contestant and Bachelor Info
season <- left_join(contestant, bachelor, by="season")

# Calculate distance between bachelors and contestants
season$distance <- distHaversine(season[,c("lon","lat")],season[,c("lon_bach","lat_bach")])/1609.34

# Calculate difference in ages
season$age_diff <- season$age-season$age_bach

# Create outcome variables (winner and top 4)
season <- season %>%
  mutate(win=ifelse(name==winner,1,0),
         p=1) %>%
  group_by(season) %>%
  mutate(finish=cumsum(p)) %>%
  ungroup() %>%
  mutate(top4=ifelse(finish<=4,1,0))

# Remove Seasons 9 and 12 (Skew distance)
season <- filter(season, season!=9 & season!=12)

# Write Full Data Set
write.csv(season, "season_prepped.csv", row.names = FALSE)

write.csv(bachelor, "bachelor_prepped.csv", row.names = FALSE)

write.csv(contestant, "contestant_prepped.csv", row.names = FALSE)





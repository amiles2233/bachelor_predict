library(plyr)
library(dplyr)
library(ggplot2)
library(ggmap)
library(scales)
library(labeling)
library(reshape2)


contestant <- read.csv("contestant_prepped.csv", stringsAsFactors = FALSE)
bachelor <- read.csv("bachelor_prepped.csv", stringsAsFactors = FALSE)
season <- read.csv("season_prepped.csv", stringsAsFactors = FALSE)
s21 <- read.csv("s21_prepped.csv", stringsAsFactors = FALSE)


contestant$current <- ifelse(contestant$season==21,"Current","Previous")
bachelor$current <- ifelse(bachelor$season==21,"Current","Previous")
season$current <- ifelse(season$season==21,"Current","Previous")

# Age Plots ----------
# Contestants
ggplot(contestant, aes(x=age, fill=current)) +
  geom_density(alpha=.5) +
  scale_fill_manual(values=c("red4","ivory2"), name="Season") +
  theme_minimal() +
  scale_y_continuous(labels=percent) +
  xlab("Age") +
  ylab("Density") +
  ggtitle("Age Distribution of Bachelor Contestants")

# Bachelors
ggplot(bachelor, aes(x=age_bach, fill=current)) +
  geom_histogram(color="black") +
  scale_fill_manual(values=c("red4","ivory2"), name="Season") +
  theme_minimal() +
  scale_y_continuous(breaks=c(0,1,2)) +
  scale_x_continuous(breaks=seq(25,38,by=2)) +
  xlab("Age") +
  ylab("Bachelors") +
  ggtitle("Ages of Bachelors")

# Graphing Occupations -------------
contest_occ <- contestant %>%
  select(season, current, manager:owner) %>%
  group_by(season, current) %>%
  summarize_each(funs(sum)) %>%
  ungroup() %>%
  select(-season) %>%
  group_by(current) %>%
  summarise_each(funs(mean)) %>%
  melt()

ggplot(contest_occ, aes(x=variable, y=value, fill=current)) +
  geom_bar(position="dodge", stat="identity", color="black") +
  scale_fill_manual(values=c("red4","ivory2"), name="Season") +
  theme_minimal() +
  xlab("Occupations") +
  ylab("Contestants") +
  ggtitle("Bachelor Contestant Occupations",
          subtitle="Previous Seasons Averaged")

quantile(contestant$lon, probs = c(.01,.99))  
quantile(contestant$lat, probs = c(.01,.99))  

contestLoc <- c(-130,22,-60,55)
map <- get_map(contestLoc, source="stamen", maptype = "toner")

ggmap(map) +
  geom_point(data=contestant, aes(x=lon, y=lat, color=current)) +
  theme_void() +
  scale_color_manual(values=c("red3","rosybrown3"), name="Season") +
  ggtitle("Bachelor Contestant Locations")


## Plotting Distance
ggplot(season, aes(x=distance, fill=current)) +
  geom_density(alpha=.5) +
  scale_fill_manual(values=c("red4","ivory2"), name="Season") +
  theme_minimal() +
  scale_y_continuous(labels=percent) +
  scale_x_continuous(labels=comma) +
  xlab("Distance (Miles)") +
  ylab("Density") +
  ggtitle("Bachelor-Contestant Distance Distribution")


## Plotting Outcome Probabilities
ggplot(s21, aes(x=win, y=top4, label=name)) +
  geom_point() +
  geom_label(size=2.5, vjust=-.1, hjust=-.1) +
  scale_y_continuous(labels=percent, limits=c(0,.65)) +
  scale_x_continuous(labels=percent, limits=c(0,.65)) +
  xlab("Probability of Getting Final Rose") +
  ylab("Probability of Getting Hometown Date") +
  ggtitle("Bachelor Outcome Probabilities")

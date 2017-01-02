library(plyr)
library(dplyr)
library(caret)
library(ggplot2)
library(scales)
library(labeling)
library(randomForest)

season <- read.csv("season_prepped.csv", stringsAsFactors = FALSE)

# Only use Modeling Vars
season <- select(season, name, age, season, region.1:owner, distance, age_diff, win, top4)

# Split training and testing sets
train <- filter(season, season<21) %>%
  mutate(win=factor(win, levels=c(1,0),labels=c("Win","Lose")),
         top4=factor(top4, levels=c(1,0),labels=c("Top4","NotTop4")))

train <- train[complete.cases(train),]
test <- filter(season, season==21) %>%
  select(-win, -top4)

fitControl <- trainControl(method="boot",
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)

top4Fit <- train(top4 ~ age + manager + teacher + nurse + student + owner +
                   sales + attorney + model + real_estate + region.1 +
                   region.2 + region.3 + region.4 + region.5 +
                   region.6 + region.7 + distance + age_diff,
                data=train,
                trControl=fitControl,
                method="rf",
                metric="ROC")

winFit <- train(win ~ age + manager + teacher + nurse + student + owner +
                  sales + attorney + model + real_estate + region.1 +
                  region.2 + region.3 + region.4 + region.5 +
                  region.6 + region.7 + distance + age_diff,
                 data=train,
                 trControl=fitControl,
                 method="rf",
                 metric="ROC")

test$win <- predict(winFit, newdata = test, type="prob")$Win
test$top4 <- predict(top4Fit, newdata = test, type="prob")$Top4

write.csv(test, "s21_prepped.csv", row.names = FALSE)




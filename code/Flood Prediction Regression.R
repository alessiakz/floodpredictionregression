here::i_am("code/Flood Prediction Regression.R")

# load libraries
library(tidyverse)
library(stats)
library(ggplot2)
library(corrplot)
library(Hmisc)

# load datasets
train <- read.csv(here::here("data/train.csv"), header = TRUE)
test <- read.csv(here::here("data/test.csv"), header = TRUE)
sample <- read.csv(here::here("data/sample_submission.csv"), header = TRUE)

# exploring data
# correlation matrix
corr_matrix <- cor(train)
round(corr_matrix, 2)

#top variables:
  #MonsoonIntensity
  #TopographyDrainage
  #RiverManagement
  #DamsQuality
  #Siltation
  #DeterioratingInfrastructure
  #PopulationScore

# multiple linear regression

  ## all variables
  model <- lm(FloodProbability ~. -id, data = train)
  summary(model)
  confint(model)
  
    ### checking model
    sigma(model)/mean(train$FloodProbability)
      #4% error rate

# applying model to test dataset
test$FloodProbability <- predict(model, newdata=test)

submission <- test %>% 
  select(id, FloodProbability)

# exporting
write.csv(submission, here::here("output/submission.csv"), row.names = FALSE)

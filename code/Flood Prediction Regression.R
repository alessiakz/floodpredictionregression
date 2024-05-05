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
round(corr_matrix, 3)

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
    ### r squared
    summary(model)$r.squared 
    
  ## select variables
  # model_select <- lm(FloodProbability ~ MonsoonIntensity + TopographyDrainage + RiverManagement +
  #                                       Deforestation + Urbanization + ClimateChange + DamsQuality +
  #                                       Siltation + AgriculturalPractices + Encroachments + 
  #                                       IneffectiveDisasterPreparedness +
  #                                       DrainageSystems + CoastalVulnerability + Landslides + Watersheds +
  #                                       DeterioratingInfrastructure + PopulationScore + WetlandLoss +
  #                                       InadequatePlanning + PoliticalFactors,
  #                    data = train)
  
    model_select <- lm(FloodProbability ~ MonsoonIntensity + TopographyDrainage + 
                                        DamsQuality +
                                        Landslides +
                                        DeterioratingInfrastructure + PopulationScore,
                     data = train)
  
  summary(model_select)$r.squared
  
# multiple logistic regression
  model_logi <- glm(FloodProbability ~. -id, family=gaussian(link = "identity"), data=train)
  
  summary(model_logi)
  
# applying model to test dataset
test$FloodProbability <- predict(model_logi, newdata=test)

submission <- test %>% 
  select(id, FloodProbability)

# exporting
write.csv(submission, here::here("output/submission7.csv"), row.names = FALSE)

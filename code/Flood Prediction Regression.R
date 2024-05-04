here::i_am("code/Flood Prediction Regression.R")

# load libraries
library(tidyverse)
library(stats)

# load datasets
train <- read.csv(here::here("data/train.csv"), header = TRUE)
test <- read.csv(here::here("data/test.csv"), header = TRUE)
sample <- read.csv(here::here("data/sample_submission.csv"), header = TRUE)

# unsupervised machine learning

## k-means clustering

cl <- kmeans(train, centers = 3, nstart = 10)
plot(train, col = cl$cluster)


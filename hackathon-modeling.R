################################################
# Building & Evaluating Risk Prediction Models #
#           SIBS Hackathon Project             #
################################################

# Load in libraries
library(tidyverse)
library(caret)
library(glmnet)

# Load in data
load(file='data-files/mi_comp_data_cleaned.RData')

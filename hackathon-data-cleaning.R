# Load in Libraries
library(tidyverse)
install.packages('mice')
library(mice)

# Import Data
mi_comp <- read.csv("C:/Users/conno/Downloads/Myocardial infarction complications Database.csv")
# save(mi_comp,file='mi_comp_data_original.RData')
md.pairs(mi_comp)

# Look at data structure
str(mi_comp)

# Examine Missing Data Summary
na_list <- is.na(mi_comp) %>% 
  colSums()
names <- names(na_list)
nas <- bind_cols(names, na_list)
names(nas) <- c('variable', 'number_NAs')
nas

write_excel_csv(nas, 'na_list.csv')

nas %>% 
  arrange(-number_NAs)

# Handle Missing Data...how?

# Remove Variables Not Needed
## First remove other response variables
other_response_vars <-  
  c('FIBR_PREDS', 'PREDS_TAH', 'JELUD_TAH',
         'FIBR_JELUD', 'A_V_BLOK', 'OTEK_LANC',
         'RAZRIV', 'DRESSLER', 'ZSN',
         'P_IM_STEN', 'LET_IS')
mi_comp_clean <- mi_comp %>% 
  select(-ID, -KFK_BLOOD, -IBS_NASL, -S_AD_KBRIG, -D_AD_KBRIG,
         -NOT_NA_KB, -LID_KB, -NA_KB, -GIPER_NA, -NA_BLOOD,
         -K_BLOOD, -GIPO_K, -AST_BLOOD, -other_response_vars,
         -ALT_BLOOD, -S_AD_ORIT, -D_AD_ORIT, -ROE, -TIME_B_S,
         -82:-50, -26:-13, -DLIT_AG) %>% 
  remove_missing()

#save(mi_comp_clean, file='data-files/mi_comp_data_cleaned.RData')
# Refactor Variables for Exploratory Analysis & Plots
# Done in the data-refactor-string file

model <- glm(REC_IM ~ ., family='binomial', data=test)
summary(model)
null_model <- glm(REC_IM ~ 1, data=test, family='binomial')
step_model <- MASS::stepAIC(model, direction='backward')

data_age <- mi_comp %>% 
  select(AGE, other_response_vars) %>% 
  remove_missing()
age_model <- lm(AGE ~ ., data=data_age)
summary(age_model)

age_pred <- predict(age_model, mi_comp$other_response_vars)

cbind(mi_comp$AGE, age_pred)
mean(age_pred)
mean(mi_comp$AGE, na.rm=TRUE)

age_replace <- mi_comp %>% 
  filter(is.na(AGE) == TRUE) %>% 
  select(ID, AGE) %>% 
  mutate(AGE=age_pred[ID])

mi_comp$AGE[which(mi_comp$ID %in% age_replace$ID)] <- age_replace$AGE
mi_comp %>% 
  filter(ID==71)
mean(mi_comp$AGE)

mi_comp <- mi_comp %>%  mutate(NOT_NA_n = if_else(
  NOT_NA_1_n == 0 & NOT_NA_2_n == 0 & NOT_NA_3_n == 0,
  0, 1))

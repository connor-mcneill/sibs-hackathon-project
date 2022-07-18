# Load in Libraries
library(tidyverse)

# Import Data
mi_comp <- read.csv("C:/Users/conno/Downloads/Myocardial infarction complications Database.csv")
# save(mi_comp,file='mi_comp_data_original.RData')

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
test <- mi_comp %>% 
  select(-ID, -KFK_BLOOD, -IBS_NASL, -S_AD_KBRIG, -D_AD_KBRIG,
         -NOT_NA_KB, -LID_KB, -NA_KB, -GIPER_NA, -NA_BLOOD,
         -K_BLOOD, -GIPO_K, -AST_BLOOD)
# Refactor Variables for Exploratory Analysis & Plots
# Done in the data-refactor-string file

summary(glm(REC_IM ~ ., family='binomial', data=test))

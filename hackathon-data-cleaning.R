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
         -82:-55, -53:-50, -26:-13, -L_BLOOD, -DLIT_AG, -54) %>% 
  remove_missing()

#save(mi_comp_clean, file='data-files/mi_comp_data_cleaned.RData')
# Refactor Variables for Exploratory Analysis & Plots
# Done in the data-refactor-string file

model <- glm(REC_IM ~ ., family='binomial', data=mi_comp_clean)
summary(model)
null_model <- glm(REC_IM ~ 1, data=mi_comp_clean, family='binomial')
step_model <- MASS::stepAIC(model, direction='both')
step(model)

# Option 1: Reduce dimensionality of ordinal med variables
mi_comp_clean <- mi_comp_clean %>%  
  mutate(NOT_NA_n = if_else(
  NOT_NA_1_n == 0 & NOT_NA_2_n == 0 & NOT_NA_3_n == 0,
  0, 1)) %>% 
  select(-NOT_NA_1_n, -NOT_NA_2_n, -NOT_NA_3_n) %>% 
  mutate(ZSN_A = if_else(ZSN_A == 0, 0, 1)) %>% 
  mutate(NA_R_n = if_else(
    NA_R_1_n == 0 & NA_R_2_n == 0 & NA_R_3_n == 0,
    0, 1)) %>% 
  select(-NA_R_1_n, -NA_R_2_n, -NA_R_3_n)
  
# Option 2: Make med variables binary
mi_comp_clean <- mi_comp_clean %>% 
  mutate(NOT_NA_1_n = if_else(NOT_NA_1_n == 0, 0, 1),
         NOT_NA_2_n = if_else(NOT_NA_2_n == 0, 0, 1),
         NOT_NA_3_n = if_else(NOT_NA_3_n == 0, 0, 1),
         NA_R_1_n = if_else(NA_R_1_n == 0, 0, 1),
         NA_R_2_n = if_else(NA_R_2_n == 0, 0, 1),
         NA_R_3_n = if_else(NA_R_3_n == 0, 0, 1))

step_data <- mi_comp %>% 
  select(AGE, STENOK_AN, endocr_01, endocr_03, zab_leg_01,
         zab_leg_03, SVT_POST, GT_POST, lat_im, R_AB_3_n,
         NA_R_2_n, ANT_CA_S_n, GEPAR_S_n, TRENT_S_n, REC_IM) %>% 
  remove_missing() %>% 
  mutate(NA_R_2_n = if_else(NA_R_2_n == 0, 0, 1))

# Look at models
summary(step_model)
summary(glm(REC_IM ~ AGE + STENOK_AN + endocr_01 + zab_leg_01 +
            GT_POST + lat_im + R_AB_3_n + NA_R_2_n + ANT_CA_S_n +
              GEPAR_S_n + TRENT_S_n, family='binomial', data=mi_comp_clean))

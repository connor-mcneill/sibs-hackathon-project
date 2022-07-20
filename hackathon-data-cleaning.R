# Load in Libraries
library(tidyverse)
library(mice)

# Import Data
mi_comp <- read.csv("D:/NCSU/Summer2022/SIBS/SIBS_HackAThon/Myocardial infarction complications Database.csv")
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

step_model2 <- MASS::stepAIC()

# Option 1: Reduce dimensionality of ordinal med variables
mi_comp_clean1 <- mi_comp_clean %>%  
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
mi_comp_clean2 <- mi_comp_clean %>% 
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

#---------------------Missing data-----------------------
#library(mice)

#option 1
#mi_comp_clean1NEW <- mice(mi_comp_clean1, m=10,method = 'pmm', seed = 5)

#option 2
#mi_comp_clean2NEW <- mice(mi_comp_clean2, m=10,method = 'pmm', seed = 5)

#--------------------LASSO/Elastic Net LOGISTIC REGRESSION----------------------------
library(caret)
library(glmnet)
set.seed(123)


#which( colnames(mi_comp_clean1)=="REC_IM" )
#40

###############LASSO Reg for option1: Reduce dimensionality of ordinal med variables############
inTrain <- as.vector(createDataPartition(mi_comp_clean1[,40],p=0.8,list=FALSE, times=1))
dTrain <- mi_comp_clean1[inTrain,]; dim(dTrain)
dTest <- mi_comp_clean1[-inTrain,]; dim(dTest)

#gather response var into vectors
yTrain <- dTrain[,40]
xTrain <- as.matrix(dTrain[,-40])
yTest <- dTest[,40]
xTest <- as.matrix(dTest[,-40])

#To fit a lasso logistic regression model, the only modifications needed are to specify a factor response variable and to include family="binomial" in the glmnet call:
fit.lasso.b <- glmnet(xTrain, yTrain, alpha=1, standardize=TRUE, family="binomial")
plot(fit.lasso.b, label=TRUE, xvar="lambda")


set.seed(123)
cv.lasso.b <- cv.glmnet(xTrain, yTrain, alpha=1, standardize=TRUE, family="binomial", nfolds=10)
plot(cv.lasso.b)

cv.lasso.b$lambda.min #Average mean-squared prediction error is minimized when lambda = 0.01563434; this model includes 4 predictors

cv.lasso.b$lambda.1se #The most-regularized model within one standard error of this "minimum' ' model has lambda = 0.04774502 and includes 0 predictors

#Estimated Odds Ratios
lasso.b.coef <- coef(cv.lasso.b, s=cv.lasso.b$lambda.min) # lambda.min --> if Cross Validataion Curve's second vertial line ends up with 0 variables.
lasso.b.coef #########MANY DROPPED!
#exp(lasso.b.coef) #Estimated Odds Ratios
#MANY VARIABLE IMPACT NEGATIVELY TO RESPONSE VAR.

#~~~~~~ROC LASSO Option 1~~~~~~~~
#FIRST, look at the estimated probabilities, and then convert these into"TRUE" or "FALSE":
phat <- predict(cv.lasso.b, newx=xTest[1:10,], s=cv.lasso.b$lambda.min, type="response")
yhat <- predict(cv.lasso.b, newx=xTest[1:10,], s=cv.lasso.b$lambda.min, type="class")
cbind(phat, yhat, yTest[1:10], unname(dTest[1:10,]))

#SECOND,consider many ways to convert the estimated probabilities into TRUE or FALSE
phat <- predict(cv.lasso.b, newx=xTest, s=cv.lasso.b$lambda.min, type="response")
#install.packages("ROCR")
library(ROCR)
perf <- ROCR::performance(ROCR::prediction(phat, yTest), "tpr", "fpr")
par(las=1)
plot(perf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=.1), text.adj=c(-0.2,1.7)) #plot ROC curve

#The ideal ROC curve has area-under-the-curve of one, so let's see how close we get to one:
as.numeric( ROCR::performance(ROCR::prediction(phat, yTest), "auc")@y.values ) #AUC(ROC)
#ANSWER: 0.7147619

#############LASSO Reg for option2: Make med variables binary#############
set.seed(777)
#LASSO Reg for option2
inTrain2 <- as.vector(createDataPartition(mi_comp_clean2[,40],p=0.8,list=FALSE, times=1))
dTrain2 <- mi_comp_clean2[inTrain2,]; dim(dTrain2)
dTest2 <- mi_comp_clean2[-inTrain2,]; dim(dTest2)

#gather response var into vectors
yTrain2 <- dTrain2[,40]
xTrain2 <- as.matrix(dTrain2[,-40])
yTest2 <- dTest2[,40]
xTest2 <- as.matrix(dTest2[,-40])

fit.lasso.b2 <- glmnet(xTrain2, yTrain2, alpha=1, standardize=TRUE, family="binomial")
plot(fit.lasso.b2, label=TRUE, xvar="lambda")

set.seed(777)
cv.lasso.b2 <- cv.glmnet(xTrain2, yTrain2, alpha=1, standardize=TRUE, family="binomial", nfolds=10)
plot(cv.lasso.b2)

#Estimating Odds Ratio
cv.lasso.b2$lambda.min #Average mean-squared prediction error is minimized when lambda = 0.01856392; this model includes 4 predictors

cv.lasso.b2$lambda.1se #The most-regularized model within one standard error of this "minimum' ' model has lambda = 0.047 and includes 0 predictors

#Estimated Odds Ratios
lasso.b.coef2 <- coef(cv.lasso.b2, s=cv.lasso.b2$lambda.1se) #lambda.1se (if Cross Validataion Curve's second vertial line ends up with More than 0 variables) VS. lambda.min --> if Cross Validataion Curve's second vertial line ends up with 0 variables.
lasso.b.coef2 ######LESS ARE DROPPED THAN OPTION1
#exp(lasso.b.coef2) #Estimated Odds Ratios
#LOOKS BETTER, 5 VARIABLE IS LITTLE LESS

#~~~~~~ROC LASSO Option 2~~~~~~~~
#FIRST, look at the estimated probabilities, and then convert these into"TRUE" or "FALSE":
phat <- predict(cv.lasso.b2, newx=xTest2[1:10,], s=cv.lasso.b2$lambda.1se, type="response")
yhat <- predict(cv.lasso.b2, newx=xTest2[1:10,], s=cv.lasso.b2$lambda.1se, type="class")
cbind(phat, yhat, yTest2[1:10], unname(dTest2[1:10,]))

#SECOND,consider many ways to convert the estimated probabilities into TRUE or FALSE
phat <- predict(cv.lasso.b2, newx=xTest2, s=cv.lasso.b2$lambda.1se, type="response")
#install.packages("ROCR")
library(ROCR)
perf <- ROCR::performance(ROCR::prediction(phat, yTest2), "tpr", "fpr")
par(las=1)
plot(perf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=.1), text.adj=c(-0.2,1.7)) #plot ROC curve

#The ideal ROC curve has area-under-the-curve of one, so let's see how close we get to one:
as.numeric( ROCR::performance(ROCR::prediction(phat, yTest2), "auc")@y.values ) #AUC(ROC)
#ANSWER: 0.7223995

###############Elsatic Net Reg for option1: Reduce dimensionality of ordinal med variables############
inTrain3 <- as.vector(createDataPartition(mi_comp_clean1[,40],p=0.8,list=FALSE, times=1))
dTrain3 <- mi_comp_clean1[inTrain3,]; dim(dTrain3)
dTest3 <- mi_comp_clean1[-inTrain3,]; dim(dTest3)

#gather response var into vectors
yTrain3 <- dTrain3[,40]
xTrain3 <- as.matrix(dTrain3[,-40])
yTest3 <- dTest3[,40]
xTest3 <- as.matrix(dTest3[,-40])

#To fit a Ridge logistic regression model, the only modifications needed are to specify a factor response variable and to include family="binomial" in the glmnet call:
fit.lasso.b3 <- glmnet(xTrain3, yTrain3, alpha=0.5, standardize=TRUE, family="binomial")
plot(fit.lasso.b3, label=TRUE, xvar="lambda")


set.seed(123)
cv.lasso.b3 <- cv.glmnet(xTrain3, yTrain3, alpha=0.5, standardize=TRUE, family="binomial", nfolds=10)
plot(cv.lasso.b3)

cv.lasso.b3$lambda.min #Average mean-squared prediction error is minimized when lambda = 0.01856392; this model includes 4 predictors

cv.lasso.b3$lambda.1se #The most-regularized model within one standard error of this "minimum' ' model has lambda = 48.89813 and includes 0 predictors

#Estimated Odds Ratios
lasso.b.coef3 <- coef(cv.lasso.b3, s=cv.lasso.b3$lambda.1se)
lasso.b.coef3 ###########NO GOOD... EVERYTHING DROPPED.
#exp(lasso.b.coef3) #Estimated Odds Ratios

#~~~~~~ROC Elastic Net Option 1~~~~~~~~
#FIRST, look at the estimated probabilities, and then convert these into"TRUE" or "FALSE":
phat <- predict(cv.lasso.b3, newx=xTest3[1:10,], s=cv.lasso.b3$lambda.min, type="response")
yhat <- predict(cv.lasso.b3, newx=xTest3[1:10,], s=cv.lasso.b3$lambda.min, type="class")
cbind(phat, yhat, yTest3[1:10], unname(dTest3[1:10,]))

#SECOND,consider many ways to convert the estimated probabilities into TRUE or FALSE
phat <- predict(cv.lasso.b3, newx=xTest3, s=cv.lasso.b3$lambda.min, type="response")
#install.packages("ROCR")
library(ROCR)
perf <- ROCR::performance(ROCR::prediction(phat, yTest3), "tpr", "fpr")
par(las=1)
plot(perf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=.1), text.adj=c(-0.2,1.7)) #plot ROC curve

#The ideal ROC curve has area-under-the-curve of one, so let's see how close we get to one:
as.numeric( ROCR::performance(ROCR::prediction(phat, yTest3), "auc")@y.values ) #AUC(ROC)
#ANSWER: 0.5 TOO.

#############Elastic Net Reg for option2: Make med variables binary#############
set.seed(777)
#LASSO Reg for option2
inTrain4 <- as.vector(createDataPartition(mi_comp_clean2[,40],p=0.8,list=FALSE, times=1))
dTrain4 <- mi_comp_clean2[inTrain4,]; dim(dTrain2)
dTest4 <- mi_comp_clean2[-inTrain4,]; dim(dTest2)

#gather response var into vectors
yTrain4 <- dTrain4[,40]
xTrain4 <- as.matrix(dTrain4[,-40])
yTest4 <- dTest4[,40]
xTest4 <- as.matrix(dTest4[,-40])

fit.lasso.b4 <- glmnet(xTrain4, yTrain4, alpha=0.5, standardize=TRUE, family="binomial")
plot(fit.lasso.b4, label=TRUE, xvar="lambda")

set.seed(777)
cv.lasso.b4 <- cv.glmnet(xTrain4, yTrain4, alpha=0.5, standardize=TRUE, family="binomial", nfolds=10)
plot(cv.lasso.b4)

#Estimating Odds Ratio
cv.lasso.b4$lambda.min #Average mean-squared prediction error is minimized when lambda = 0.01856392; this model includes 4 predictors

cv.lasso.b4$lambda.1se #The most-regularized model within one standard error of this "minimum' ' model has lambda = 0.047 and includes 0 predictors

#Estimated Odds Ratios
lasso.b.coef4 <- coef(cv.lasso.b4, s=cv.lasso.b4$lambda.1se)
lasso.b.coef4 #####MAJORITY ARE DROPPED!
#exp(lasso.b.coef4) #Estimated Odds Ratios
#BEST MODEL?! 9 VARIABLE LOOKS RIGHT.

#~~~~~~ROC Elastic Net Option 2~~~~~~~~
#FIRST, look at the estimated probabilities, and then convert these into"TRUE" or "FALSE":
phat <- predict(cv.lasso.b4, newx=xTest4[1:10,], s=cv.lasso.b4$lambda.1se, type="response")
yhat <- predict(cv.lasso.b4, newx=xTest4[1:10,], s=cv.lasso.b4$lambda.1se, type="class")
cbind(phat, yhat, yTest4[1:10], unname(dTest4[1:10,]))

#SECOND,consider many ways to convert the estimated probabilities into TRUE or FALSE
phat <- predict(cv.lasso.b4, newx=xTest4, s=cv.lasso.b4$lambda.1se, type="response")
#install.packages("ROCR")
library(ROCR)
perf <- ROCR::performance(ROCR::prediction(phat, yTest4), "tpr", "fpr")
par(las=1)
plot(perf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=.1), text.adj=c(-0.2,1.7)) #plot ROC curve

#The ideal ROC curve has area-under-the-curve of one, so let's see how close we get to one:
as.numeric( ROCR::performance(ROCR::prediction(phat, yTest4), "auc")@y.values ) #AUC(ROC)
#ANSWER: 0.7230496

#########The conclusion is that the model seems to have reasonable performance. Ultimately, you would compare AUC(ROC) for several competing models.

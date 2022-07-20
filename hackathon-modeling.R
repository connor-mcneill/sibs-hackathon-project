################################################
# Building & Evaluating Risk Prediction Models #
#           SIBS Hackathon Project             #
################################################

# Load in libraries
library(tidyverse)
library(caret)
library(glmnet)

# Load in data
load(file='data-files/mi_comp_data_bin.RData')

# ------------- Model 1: Stepwise Logistic Regression ---------
model <- glm(REC_IM ~ ., family='binomial', data=mi_comp_clean2)
summary(model)
step_model <- MASS::stepAIC(model, direction='both')

summary(step_model)

library(pROC)
p.hats <- predict(step_model, type='response')
mi_comp_clean2$p.hats <- p.hats
roc.mod <- roc(mi_comp_clean2$REC_IM, mi_comp_clean2$p.hats)
plot.roc(roc.mod)
auc(roc.mod)

# ------------- Create Train & Test Sets ----------------------
set.seed(614)
inTrain <- as.vector(caret::createDataPartition(mi_comp_clean2[,46], p=.8, list=FALSE, times=1))
dTrain <- mi_comp_clean2[inTrain,]; dim(dTrain)

dTest <- mi_comp_clean2[-inTrain,]; dim(dTest)

yTrain <- dTrain[,46]
xTrain <- as.matrix(dTrain[,-46])
yTest <- dTest[,46]
xTest <- as.matrix(dTest[,-46])

mean(mi_comp_clean2[,46] == mi_comp_clean2$REC_IM)

table(yTrain); table(yTest)

# ------------- Model 2: LASSO Logistic Regression ------------
fit.lasso <- glmnet(xTrain, yTrain, alpha=1, standardize=TRUE, family='binomial')
plot(fit.lasso, label=TRUE, xvar='lambda')

set.seed(3201)
cv.lasso <- cv.glmnet(xTrain, yTrain, alpha=1, standardize=TRUE,
                      family='binomial', nfolds=10)
plot(cv.lasso)

cv.lasso$lambda.min

cv.lasso$lambda.1se

cv.lasso$lambda.mid <- mean(c(cv.lasso$lambda.min, cv.lasso$lambda.mid))

lasso.coef <- coef(cv.lasso, s=0.017)

phat <- predict(cv.lasso, newx=xTest[1:10,], s=0.017, type="response")
yhat <- predict(cv.lasso, newx=xTest[1:10,], s=0.017, type="class")
cbind(phat, yhat, yTest[1:10], unname(dTest[1:10,]))

phat <- predict(cv.lasso, newx=xTest, s=0.017, type="response")
#install.packages("ROCR")
library(ROCR)
perf <- ROCR::performance(ROCR::prediction(phat, yTest), "tpr", "fpr")
par(las=1)
plot(perf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=.1), text.adj=c(-0.2,1.7)) #plot ROC curve

as.numeric( ROCR::performance(ROCR::prediction(phat, yTest), "auc")@y.values ) #AUC(ROC)

# ------------- Model 3: Elastic Net Logistic Regression ------
fit.elastic <- glmnet(xTrain, yTrain, alpha=.5, standardize=TRUE, family='binomial')
plot(fit.elastic, label=TRUE, xvar='lambda')

set.seed(3201)
cv.elastic <- cv.glmnet(xTrain, yTrain, alpha=.5, standardize=TRUE,
                        family='binomial', nfolds=10)
plot(cv.elastic)

coef()
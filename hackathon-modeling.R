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
load(file='data-files/completedData.RData')



# ------------- Model 1: Stepwise Logistic Regression ---------
model <- glm(REC_IM ~ ., family='binomial', data=completed_clean)
summary(model)
step_model <- MASS::stepAIC(model, direction='both')

summary(stepmodel)

stepmodel <- glm(formula = REC_IM ~ AGE + STENOK_AN + IBS_POST + GB + DLIT_AG + 
                   zab_leg_01 + GT_POST + ritm_ecg_p_07 + L_BLOOD + 
                   R_AB_3_n + NA_R_2_n + ANT_CA_S_n + GEPAR_S_n + TIKL_S_n + 
                   TRENT_S_n, family = "binomial", data = completed_clean)

step_model2 <- glm(REC_IM ~ AGE + STENOK_AN + IBS_POST +
              GB + DLIT_AG + lat_im + L_BLOOD +
              R_AB_2_n + R_AB_3_n + NOT_NA_3_n + ANT_CA_S_n +
              GEPAR_S_n,
              family ='binomial', data=mi_comp_clean)

step_model1 <- glm(formula = REC_IM ~ AGE + STENOK_AN + endocr_01 + zab_leg_01 + 
      GT_POST + lat_im + R_AB_3_n + NA_R_2_n + ANT_CA_S_n + GEPAR_S_n + 
      TRENT_S_n, family = "binomial", data = mi_comp_clean)

exp(coef(step_model))

summary(step_model0)

summary(stepmodel)
library(pROC)
p.hats <- predict(stepmodel, newdata=completed_clean, type='response')
completed_clean$p.hats <- p.hats
roc.mod <- roc(completed_clean$REC_IM, completed_clean$p.hats)
plot.roc(roc.mod)
auc(roc.mod)
coef(stepmodel)
# install.packages('forestplot')
library(forestplot)
exp(confint(stepmodel))[,1]
variable_list <- as.character(1:15)
variable_list <- c('Age', 'Chest Pain History', 'Recent CHD',
                   'Essential HTN', 'Arterial HTN Duration', 'COPD',
                   'Paroxymal VT', 'Tachycardia', 'WBC Count',
                   'Relapse Pain Day3', 'Opioids Given Day2',
                   'Calcium Blockers Given', 'Anticoagulants Given',
                   'Ticlid Used', 'Trent Used')
base_data <- tibble(mean= exp(coef(stepmodel))[-1],
                      lower= exp(confint(stepmodel))[-1,1],
                      upper= exp(confint(stepmodel))[-1,2],
                      variables= variable_list,
                      or=as.character(round(exp(coef(stepmodel))[-1], 4)),
                      p.values=coef(summary(stepmodel))[-1,4])
header <- tibble(#variables='Covariates',
                 or='Odds Ratio',
                 p.values='P-value')
library(forcats)

forest_data <- bind_rows(header, base_data)
forest_data %>% 
  forestplot(labeltext=c(or, p.values),
             txt_gp = fpTxtGp(label = gpar(fontfamily = "", cex=.5)),
             clip=c(0,2.5))
base_data %>% 
  mutate(variables=fct_reorder(variables, desc(p.values))) %>% 
  ggplot()+
  geom_pointrange(aes(col=variables, y=variables, x=mean, xmin=lower, xmax=upper), show.legend=FALSE) +
#  geom_vline(xintercept=1, linetype='dashed', cex=.5) +
  geom_text(aes(label=round(mean,4), x=mean, y=variables),size=3, nudge_y=0.35) +
  scale_x_log10() +
  labs(x='Odds Ratio', y='Covariates\n(ranked by p-value)')

storage <- coef(summary(stepmodel))

perf <- ROCR::performance(ROCR::prediction(completed_clean$p.hats, completed_clean$REC_IM), "tpr", "fpr")
par(las=1)
plot(perf, colorize=TRUE, text.adj=c(-0.2,1.7)) #plot ROC curve

as.numeric( ROCR::performance(ROCR::prediction(completed_clean$p.hats, completed_clean$REC_IM), "auc")@y.values ) #AUC(ROC)

dec <- quantile(completed_clean$p.hats, probs=seq(0,1,by=0.1), type=3) 
completed_clean$dec_grp <- cut(completed_clean$p.hats,         # Predicted probabilities (var to group)
                   breaks = dec,       # Cut points (group intervals)
                   include.lowest = T, # Include smallest value 
                   labels = 1:10)
prop.table(table(completed_clean$dec_grp))
agg <- aggregate(cbind(REC_IM,p.hats) ~ dec_grp, # Aggregate A,B by C
                 data = completed_clean,                  # From data set
                 FUN = 'mean')                # Using this summary function 
agg

cal.fit <- lm(REC_IM ~ p.hats, data=agg)
agg$fit <- predict(cal.fit)
summary(cal.fit)

ggplot(agg) +
  geom_point(aes(x=p.hats, y=REC_IM), col='#F8766D') +
  labs(x='Predicted Probabilities', y='Observed Event Rate') +
  geom_abline(slope=1, yintercept=0, lwd=.7) +
  geom_line(aes(x=p.hats,y=fit), linetype='dashed', color='#619CFF',
            lwd=1.5)

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
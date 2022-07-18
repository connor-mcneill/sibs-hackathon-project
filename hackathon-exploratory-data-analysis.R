# Load in datasets
load('data-files/mi_comp_data_original.RData')
load('data-files/mi_comp_data_str.RData')

# Load in Libraries
# install.packages("corrplot")
# install.packages("ggcorrplot")
library(tidyverse)
library(corrplot)
library(ggcorrplot)

# First, look at response variables correlation
response_vars <- mi_comp %>% 
  select(FIBR_PREDS, PREDS_TAH, JELUD_TAH,
         FIBR_JELUD, A_V_BLOK, OTEK_LANC,
         RAZRIV, DRESSLER, ZSN, REC_IM,
         P_IM_STEN)

res <- cor(response_vars)
names_plot <- c("AFib", "SVT", "VT", "VFib",
                "AV Block", "PE", "Myo Rupture",
                "Dressler", "CHF", 
                "Relapse MI", "PIA")
rownames(res) <- names_plot
colnames(res) <- names_plot
#cor(response_vars$REC_IM, response_vars$A_V_BLOK)
#response_vars %>% head()
#corrplot(res, type='upper', order='hclust',
#         tl.col='black', tl.srt=45)
#plot(corr_plot1)
#summary(glm(REC_IM ~ ., data=response_vars, family='binomial'))
#col <- colorRampPalette(c("white", "blue", "red"))(20)
#heatmap(res, col=col, Colv=NA, Rowv=NA)
corr_plot <- ggcorrplot(res, method='circle')

#?heatmap
#table(mi_comp_chr$REC_IM, mi_comp_chr$OTEK_LANC)
#40/119
#119/1422

#cov(mi_comp$REC_IM, mi_comp$OTEK_LANC)

#mi_comp_chr %>% 
#  ggplot() +
#  geom_boxplot(aes(y=AGE, x=REC_IM, fill=REC_IM))

#mi_comp_chr %>% 
#  ggplot() +
#  geom_bar(aes(x=REC_IM, fill=REC_IM))

#mi_comp_chr %>% 
#  ggplot() +
#  geom_bar(aes(x=REC_IM, fill=SEX), position='fill')

summary(glm(REC_IM ~ DLIT_AG, family='binomial', data=mi_comp))

# DLIT_AG appears significant. Will need to look at missing data.
mi_comp %>% 
  ggplot() +
  geom_boxplot(aes(y=DLIT_AG, x=factor(REC_IM), fill=factor(REC_IM)))

#table(mi_comp_chr$DLIT_AG, mi_comp_chr$REC_IM)

# ritm_ecg_p_07 may be significant
prop.test(x=c(84, 981), n=c(159, 1541))

table(mi_comp_chr$ritm_ecg_p_07, mi_comp_chr$REC_IM)

mi_comp_chr %>% 
  ggplot() +
  geom_violin(aes(y=L_BLOOD, x=REC_IM, fill=REC_IM))

summary(glm(REC_IM ~ L_BLOOD, data=mi_comp, family='binomial'))

table(mi_comp_chr$n_p_ecg_p_12, mi_comp_chr$REC_IM)
prop.test(x=c(5,73), n=c(140, 1445))


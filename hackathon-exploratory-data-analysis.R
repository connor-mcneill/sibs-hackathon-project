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



#table(mi_comp_chr$SEX, mi_comp_chr$REC_IM)
#.04411765/(.04941176+.04411765)
#.04941176/(.04941176+.04411765)

#(0.04411765/(0.32941176+0.04411765))/.1969834
#(0.04941176/(0.57705882+0.04941176))/.1969834
#.1181102+.07887323

#mi_comp_chr %>% 
#  ggplot() +
#  geom_boxplot(aes(y=AGE, x=REC_IM, fill=REC_IM))

#mi_comp_chr %>% 
#  ggplot() +
#  geom_bar(aes(x=REC_IM, fill=REC_IM))

#mi_comp_chr %>% 
#  ggplot() +
#  geom_bar(aes(x=REC_IM, fill=SEX), position='fill')

# summary(lm(REC_IM ~ ., data=mi_comp[,2:124]))

# summary(lm(REC_IM ~ NOT_NA_KB, data=mi_comp))
# mi_comp_chr %>% 
#  ggplot() +
#  geom_boxplot(aes(y=AST_BLOOD, x=REC_IM, fill=REC_IM))+
#  scale_y_log10()
# table(mi_comp_chr$AST_BLOOD, mi_comp_chr$REC_IM)
# 63/564
# 30/366
# prop.test(x=c(75, 49), n=c(722, 485))

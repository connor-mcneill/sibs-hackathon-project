# Load in datasets
load('data-files/mi_comp_data_original.RData')
load('data-files/mi_comp_data_str.RData')
load('data-files/mi_comp_data_cleaned.Rdata')

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

#summary(glm(REC_IM ~ DLIT_AG, family='binomial', data=mi_comp))

# DLIT_AG appears significant. Will need to look at missing data.
#mi_comp %>% 
#  ggplot() +
#  geom_boxplot(aes(y=DLIT_AG, x=factor(REC_IM), fill=factor(REC_IM)))

#table(mi_comp_chr$DLIT_AG, mi_comp_chr$REC_IM)

# ritm_ecg_p_07 may be significant
#prop.test(x=c(44, 309), n=c(44+95, 1409))

#table(mi_comp_chr$ritm_ecg_p_07, mi_comp_chr$REC_IM)

#mi_comp_chr$SEX %>% head()
#mi_comp$SEX %>% head() 

#mi_comp_chr %>% 
#  ggplot() +
#  geom_boxplot(aes(y=L_BLOOD, x=REC_IM, fill=REC_IM)) + 
#  scale_y_log10()


#lblood.model <- glm(REC_IM ~ L_BLOOD, data=mi_comp, family='binomial')
#summary(lblood.model)

#table(mi_comp_chr$n_p_ecg_p_12, mi_comp_chr$REC_IM)
#prop.test(x=c(5,73), n=c(140, 1445))

#table(mi_comp_chr$endocr_01, mi_comp_chr$REC_IM)
#prop.test(x=c(31,197), n=c(31+126, 197+1335))

#table(mi_comp_chr$SEX, mi_comp_chr$REC_IM)
#prop.test(x=c(75,84), n=c(75+560, 981+84))
#75/(75+560)
#84/(84+981)

#table(mi_comp_chr$B_BLOK_S_n, mi_comp_chr$REC_IM)
#prop.test(x=c(20, 195), n=c(138+20, 1336+195))


#ggplot() +
#  geom_bar(aes(fill=mi_comp_chr$REC_IM, x=mi_comp$STENOK_AN), position='fill') +
#  labs(x='STENOK_AN', y='Proportion', fill='REC_IM')

# ggplot() +
#   geom_boxplot(aes(y=mi_comp_chr$REC_IM, fill=mi_comp_chr$REC_IM, x=mi_comp$STENOK_AN), show.legend=FALSE) +
#   labs(x='STENOK_AN', y='REC_IM', fill='REC_IM')
# 
# mi_comp_chr %>% 
#   remove_missing(vars=c('STENOK_AN')) %>% 
#   ggplot() +
#   geom_bar(aes(x=STENOK_AN, fill=factor(REC_IM)), show.legend=FALSE) +
#   facet_wrap(facets=vars(REC_IM), scales='free', labeller=as_labeller(labels)) +
#   labs(x = 'Exertional AP Time Period (Years Since)')
# 
# labels <- c(
#   `No` = 'No Rec MI',
#   `Yes` = 'Rec MI' )
# 
# 
# mi_comp_chr %>% 
#   remove_missing(vars=c('AGE')) %>% 
#   ggplot() +
#   geom_histogram(aes(x=AGE, fill=factor(REC_IM)), bins=10, show.legend=FALSE) +
#   facet_wrap(facets=vars(REC_IM), scales='free', labeller=as_labeller(labels)) +
#   labs(x = 'Age (Years)')
# 
# mi_comp_chr %>% 
#   remove_missing(vars=c('AGE')) %>% 
#   ggplot() +
#   geom_boxplot(aes(y=REC_IM, x=AGE, fill=REC_IM), show.legend=FALSE) +
#   labs(y='Recurrent MI', x='Age (Years)') 
# 
# step_pred_corr <- mi_comp_clean %>% 
#   select(AGE, STENOK_AN, endocr_01, zab_leg_01, GT_POST,
#          lat_im, R_AB_3_n, NA_R_2_n, ANT_CA_S_n, GEPAR_S_n,
#          TRENT_S_n) %>% 
#   cor()
# 
# spc_names <- c("Age", "Exertional AP", "DM",
#                "COPD", "VTech", "Lateral MI",
#                "Pain Day 3", "Opioids Day 2", "Ca Blockers",
#                'Heparin', 'Trental')
# rownames(step_pred_corr) <- spc_names
# colnames(step_pred_corr) <- spc_names
# 
# corrplot(step_pred_corr, type='upper', order='hclust',
#          tl.col='black', tl.srt=45)
# 
# table(mi_comp_chr$REC_IM, mi_comp_chr$LET_IS)
# 
# table
# Load in libraries
library(tidyverse)

# Load in cleaned dataset
load('data-files/mi_comp_data_original.RData') # loads in as mi_comp

# Create helpful functions
replace_NY <- function(variable) {
  return(if_else(variable==0,'No','Yes'))
}

# Refactor to character values using mutate
mi_comp_chr <- mi_comp %>% 
  mutate(SEX=if_else(SEX==0, 'Female', 'Male')) %>%
  mutate(INF_ANAM=case_when(
    INF_ANAM == 0 ~ '0',
    INF_ANAM == 1 ~ '1',
    INF_ANAM == 2 ~ '2',
    INF_ANAM == 3 ~ '3+')) %>% 
  mutate(STENOK_AN=case_when(
    STENOK_AN == 0 ~ 'Never',
    STENOK_AN == 1 ~ '< 1 year',
    STENOK_AN == 2 ~ '1 year',
    STENOK_AN == 3 ~ '2 years',
    STENOK_AN == 4 ~ '3 years',
    STENOK_AN == 5 ~ '4-5 years',
    STENOK_AN == 6 ~ '> 5 years')) %>% 
  mutate(FK_STENOK=case_when(
    FK_STENOK == 0 ~ 'None',
    FK_STENOK == 1 ~ 'I FC',
    FK_STENOK == 2 ~ 'II FC',
    FK_STENOK == 3 ~ 'III FC',
    FK_STENOK == 4 ~ 'IV FC')) %>% 
  mutate(IBS_POST=case_when(
    IBS_POST == 0 ~ "None",
    IBS_POST == 1 ~ "Extertional",
    IBS_POST == 2 ~ "Unstable" )) %>% 
  mutate(IBS_NASL=if_else(IBS_NASL==0,"Isn't Burdened", "Burdened")) %>% 
  mutate(GB = case_when(
    GB == 0 ~ "None",
    GB == 1 ~ "Stage 1",
    GB == 2 ~ "Stage 2",
    GB == 3 ~ "Stage 3" )) %>% 
  mutate(SIM_GIPERT=if_else(SIM_GIPERT==0,'No', 'Yes')) %>% 
  mutate(DLIT_AG=case_when(
    DLIT_AG == 0 ~ "None",
    DLIT_AG == 1 ~ "1 year",
    DLIT_AG == 2 ~ "2 years",
    DLIT_AG == 3 ~ "3 years",
    DLIT_AG == 4 ~ "4 years",
    DLIT_AG == 5 ~ "5 years",
    DLIT_AG == 6 ~ "6-10 years",
    DLIT_AG == 7 ~ ">10 years")) %>%
  mutate(ZSN_A=case_when(
    ZSN_A == 0 ~ "None",
    ZSN_A == 1 ~ "I stage",
    ZSN_A == 2 ~ "IIA stage (right)",
    ZSN_A == 3 ~ "IIA stage (left)",
    ZSN_A == 4 ~ "IIB stage" )) %>%
  mutate(nr_11=replace_NY(nr_11)) %>%
  mutate(nr_01=replace_NY(nr_01)) %>%
  mutate(nr_02=replace_NY(nr_02)) %>%
  mutate(nr_03=replace_NY(nr_03)) %>%
  mutate(nr_04=replace_NY(nr_04)) %>%
  mutate(nr_07=replace_NY(nr_07)) %>%
  mutate(nr_08=replace_NY(nr_08)) %>%
  mutate(np_01=replace_NY(np_01)) %>%
  mutate(np_04=replace_NY(np_04)) %>%
  mutate(np_05=replace_NY(np_05)) %>%
  mutate(np_07=replace_NY(np_07)) %>%
  mutate(np_08=replace_NY(np_08)) %>%
  mutate(np_09=replace_NY(np_09)) %>%
  mutate(np_10=replace_NY(np_10)) %>%
  mutate(endocr_01=replace_NY(endocr_01)) %>%
  mutate(endocr_02=replace_NY(endocr_02)) %>%
  mutate(endocr_03=replace_NY(endocr_03)) %>%
  mutate(zab_leg_01=replace_NY(zab_leg_01)) %>%
  mutate(zab_leg_02=replace_NY(zab_leg_02)) %>%
  mutate(zab_leg_03=replace_NY(zab_leg_03)) %>%
  mutate(zab_leg_04=replace_NY(zab_leg_04)) %>%
  mutate(zab_leg_06=replace_NY(zab_leg_06)) %>%
  mutate(O_L_POST=replace_NY(O_L_POST)) %>%
  mutate(K_SH_POST=replace_NY(K_SH_POST)) %>%
  mutate(MP_TP_POST=replace_NY(MP_TP_POST)) %>%
  mutate(SVT_POST=replace_NY(SVT_POST)) %>%
  mutate(GT_POST=replace_NY(GT_POST)) %>%
  mutate(FIB_G_POST=replace_NY(FIB_G_POST)) %>%
  mutate(ant_im=case_when(
    ant_im == 0 ~ 'No infarct',
    ant_im == 1 ~ 'QRS no changes',
    ant_im == 2 ~ 'QRS like QR-complex',
    ant_im == 3 ~ 'QRS like Qr-complex',
    ant_im == 4 ~ 'QRS like QS-complex')) %>% 
  mutate(lat_im=case_when(
    lat_im == 0 ~ 'No infarct',
    lat_im == 1 ~ 'QRS no changes',
    lat_im == 2 ~ 'QRS like QR-complex',
    lat_im == 3 ~ 'QRS like Qr-complex',
    lat_im == 4 ~ 'QRS like QS-complex')) %>% 
  mutate(inf_im=case_when(
    inf_im == 0 ~ 'No infarct',
    inf_im == 1 ~ 'QRS no changes',
    inf_im == 2 ~ 'QRS like QR-complex',
    inf_im == 3 ~ 'QRS like Qr-complex',
    inf_im == 4 ~ 'QRS like QS-complex')) %>% 
  mutate(post_im=case_when(
    post_im == 0 ~ 'No infarct',
    post_im == 1 ~ 'QRS no changes',
    post_im == 2 ~ 'QRS like QR-complex',
    post_im == 3 ~ 'QRS like Qr-complex',
    post_im == 4 ~ 'QRS like QS-complex')) %>% 
  mutate(IM_PG_P=replace_NY(IM_PG_P)) %>% 
  mutate(ritm_ecg_p_01=replace_NY(ritm_ecg_p_01)) %>% 
  mutate(ritm_ecg_p_02=replace_NY(ritm_ecg_p_02)) %>% 
  mutate(ritm_ecg_p_04=replace_NY(ritm_ecg_p_04)) %>% 
  mutate(ritm_ecg_p_06=replace_NY(ritm_ecg_p_06)) %>% 
  mutate(ritm_ecg_p_07=replace_NY(ritm_ecg_p_07)) %>% 
  mutate(ritm_ecg_p_08=replace_NY(ritm_ecg_p_08)) %>% 
  mutate(n_r_ecg_p_01=replace_NY(n_r_ecg_p_01)) %>% 
  mutate(n_r_ecg_p_02=replace_NY(n_r_ecg_p_02)) %>% 
  mutate(n_r_ecg_p_03=replace_NY(n_r_ecg_p_03)) %>% 
  mutate(n_r_ecg_p_04=replace_NY(n_r_ecg_p_04)) %>% 
  mutate(n_r_ecg_p_05=replace_NY(n_r_ecg_p_05)) %>% 
  mutate(n_r_ecg_p_06=replace_NY(n_r_ecg_p_06)) %>% 
  mutate(n_r_ecg_p_08=replace_NY(n_r_ecg_p_08)) %>% 
  mutate(n_r_ecg_p_09=replace_NY(n_r_ecg_p_09)) %>% 
  mutate(n_r_ecg_p_10=replace_NY(n_r_ecg_p_10)) %>% 
  mutate(n_p_ecg_p_01=replace_NY(n_p_ecg_p_01)) %>% 
  mutate(n_p_ecg_p_03=replace_NY(n_p_ecg_p_03)) %>% 
  mutate(n_p_ecg_p_04=replace_NY(n_p_ecg_p_04)) %>% 
  mutate(n_p_ecg_p_05=replace_NY(n_p_ecg_p_05)) %>% 
  mutate(n_p_ecg_p_06=replace_NY(n_p_ecg_p_06)) %>% 
  mutate(n_p_ecg_p_07=replace_NY(n_p_ecg_p_07)) %>% 
  mutate(n_p_ecg_p_08=replace_NY(n_p_ecg_p_08)) %>% 
  mutate(n_p_ecg_p_09=replace_NY(n_p_ecg_p_09)) %>% 
  mutate(n_p_ecg_p_10=replace_NY(n_p_ecg_p_10)) %>% 
  mutate(n_p_ecg_p_11=replace_NY(n_p_ecg_p_11)) %>% 
  mutate(n_p_ecg_p_12=replace_NY(n_p_ecg_p_12)) %>% 
  mutate(fibr_ter_01=replace_NY(fibr_ter_01)) %>% 
  mutate(fibr_ter_02=replace_NY(fibr_ter_02)) %>% 
  mutate(fibr_ter_03=replace_NY(fibr_ter_03)) %>% 
  mutate(fibr_ter_05=replace_NY(fibr_ter_05)) %>% 
  mutate(fibr_ter_06=replace_NY(fibr_ter_06)) %>% 
  mutate(fibr_ter_07=replace_NY(fibr_ter_07)) %>% 
  mutate(fibr_ter_08=replace_NY(fibr_ter_08)) %>% 
  mutate(GIPO_K=replace_NY(GIPO_K)) %>% 
  mutate(GIPER_NA=replace_NY(GIPER_NA)) %>% 
  mutate(TIME_B_S=case_when(
    TIME_B_S == 1 ~ '<2 hours',
    TIME_B_S == 2 ~ '2-4 hours',
    TIME_B_S == 3 ~ '4-6 hours',
    TIME_B_S == 4 ~ '6-8 hours',
    TIME_B_S == 5 ~ '8-12 hours',
    TIME_B_S == 6 ~ '12-24 hours',
    TIME_B_S == 7 ~ '>1 day',
    TIME_B_S == 8 ~ '>2 days',
    TIME_B_S == 3 ~ '>3 days' )) %>% 
  mutate(R_AB_1_n=case_when(
    R_AB_1_n == 0 ~ 'No relapse',
    R_AB_1_n == 1 ~ 'One',
    R_AB_1_n == 2 ~ 'Two',
    R_AB_1_n == 3 ~ 'Three or more')) %>% 
  mutate(R_AB_2_n=case_when(
    R_AB_2_n == 0 ~ 'No relapse',
    R_AB_2_n == 1 ~ 'One',
    R_AB_2_n == 2 ~ 'Two',
    R_AB_2_n == 3 ~ 'Three or more')) %>% 
  mutate(R_AB_3_n=case_when(
    R_AB_3_n == 0 ~ 'No relapse',
    R_AB_3_n == 1 ~ 'One',
    R_AB_3_n == 2 ~ 'Two',
    R_AB_3_n == 3 ~ 'Three or more')) %>% 
  mutate(NA_KB=replace_NY(NA_KB)) %>% 
  mutate(NOT_NA_KB=replace_NY(NOT_NA_KB)) %>% 
  mutate(LID_KB=replace_NY(LID_KB)) %>% 
  mutate(NITR_S=replace_NY(NITR_S)) %>% 
  mutate(NA_R_1_n=case_when(
    NA_R_1_n == 0 ~ "No",
    NA_R_1_n == 1 ~ "Once",
    NA_R_1_n == 2 ~ "Twice",
    NA_R_1_n == 3 ~ "Three times",
    NA_R_1_n == 4 ~ "Four times")) %>% 
  mutate(NA_R_2_n=case_when(
    NA_R_2_n == 0 ~ "No",
    NA_R_2_n == 1 ~ "Once",
    NA_R_2_n == 2 ~ "Twice",
    NA_R_1_n == 3 ~ "Three times")) %>% 
  mutate(NA_R_3_n=case_when(
    NA_R_3_n == 0 ~ "No",
    NA_R_3_n == 1 ~ "Once",
    NA_R_3_n == 2 ~ "Twice")) %>% 
  mutate(NOT_NA_1_n=case_when(
    NOT_NA_1_n == 0 ~ "No",
    NOT_NA_1_n == 1 ~ "Once",
    NOT_NA_1_n == 2 ~ "Twice",
    NOT_NA_1_n == 3 ~ "Three times",
    NOT_NA_1_n == 4 ~ "Four times")) %>% 
  mutate(NOT_NA_2_n=case_when(
    NOT_NA_2_n == 0 ~ "No",
    NOT_NA_2_n == 1 ~ "Once",
    NOT_NA_2_n == 2 ~ "Twice",
    NOT_NA_1_n == 3 ~ "Three times")) %>% 
  mutate(NOT_NA_3_n=case_when(
    NOT_NA_3_n == 0 ~ "No",
    NOT_NA_3_n == 1 ~ "Once",
    NOT_NA_3_n == 2 ~ "Twice")) %>%
  mutate(LID_S_n=replace_NY(LID_S_n)) %>% 
  mutate(B_BLOK_S_n=replace_NY(B_BLOK_S_n)) %>% 
  mutate(ANT_CA_S_n=replace_NY(ANT_CA_S_n)) %>% 
  mutate(GEPAR_S_n=replace_NY(GEPAR_S_n)) %>% 
  mutate(ASP_S_n=replace_NY(ASP_S_n)) %>% 
  mutate(TIKL_S_n=replace_NY(TIKL_S_n)) %>% 
  mutate(TRENT_S_n=replace_NY(TRENT_S_n)) %>% 
  mutate(FIBR_PREDS=replace_NY(FIBR_PREDS)) %>% 
  mutate(PREDS_TAH=replace_NY(PREDS_TAH)) %>% 
  mutate(JELUD_TAH=replace_NY(JELUD_TAH)) %>% 
  mutate(FIBR_JELUD=replace_NY(FIBR_JELUD)) %>% 
  mutate(A_V_BLOK=replace_NY(A_V_BLOK)) %>% 
  mutate(OTEK_LANC=replace_NY(OTEK_LANC)) %>% 
  mutate(RAZRIV=replace_NY(RAZRIV)) %>% 
  mutate(DRESSLER=replace_NY(DRESSLER)) %>% 
  mutate(ZSN=replace_NY(ZSN)) %>% 
  mutate(REC_IM=replace_NY(REC_IM)) %>% 
  mutate(P_IM_STEN=replace_NY(P_IM_STEN)) %>% 
  mutate(LET_IS=case_when(
    LET_IS == 0 ~ 'Unknown',
    LET_IS == 1 ~ 'Cardiogenic shock',
    LET_IS == 2 ~ 'Pulmonary edema',
    LET_IS == 3 ~ 'Myocardial rupture',
    LET_IS == 4 ~ 'Progress of congestive heart failure',
    LET_IS == 5 ~ 'Thromboembolism',
    LET_IS == 6 ~ 'Asystole',
    LET_IS == 7 ~ 'Ventricular fibrillation'))

# save the data set w/ chr factor levels
save(mi_comp_chr,file='data-files/mi_comp_data_str.RData')

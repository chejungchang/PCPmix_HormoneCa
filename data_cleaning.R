# Data cleaning for three cancers -------------

# Library 
library(tidyverse)
library(haven)
library(stringr)

# Data import -----------
data1 <- read_sas('raw_data/dr00287_08_01.sas7bdat')


# Exposure variables -----------
# Combined Vanguard and main studies results 
# Vanguard 1=did not use, 2=less than once/month, 3=1-3 times/month, 4=1-2 times/week, 5=3-6 times/week, 6 = daily 
# Main study 1=did not use, 2=<1 time/month, 3=1-3 times/month, 4=1-5 times/week, 5=more than 5 times/week 

data2 = data1 %>%
  mutate(
    P2 = case_when(P1==0 ~ 1, P1==1 & is.na(P2) ~ 2, P1==1 & !is.na(P2) ~ P2, is.na(P1) ~ P2, TRUE ~ P2), # Eyelash mascara
    P5 = case_when(P4==0 ~ 1, P4==1 & is.na(P5) ~ 2, P4==1 & !is.na(P5) ~ P5, is.na(P4) ~ P5, TRUE ~ P5), # Eye shadow
    P9 = case_when(P8==0 ~ 1, P8==1 & is.na(P9) ~ 2, P8==1 & !is.na(P9) ~ P9, is.na(P8) ~ P9, TRUE ~ P9), # Eyeliner 
    P13 = case_when(P12==0 ~ 1, P12==1 & is.na(P13) ~ 2, P12==1 & !is.na(P13) ~ P13, is.na(P12) ~ P13, TRUE ~ P13), # Lipstick 
    P16 = case_when(P15==0 ~ 1, P15==1 & is.na(P16) ~ 2, P15==1 & !is.na(P16) ~ P16, is.na(P15) ~ P16, TRUE ~ P16), # Lip moisturizer
    P19 = case_when(P18==0 ~ 1, P18==1 & is.na(P19) ~ 2, P18==1 & !is.na(P19) ~ P19, is.na(P18) ~ P19, TRUE ~ P19), # Foundation
    P23 = case_when(P22==0 ~ 1, P22==1 & is.na(P23) ~ 2, P22==1 & !is.na(P23) ~ P23, is.na(P22) ~ P23, TRUE ~ P23), # Blush or rouge
    P27 = case_when(P26==0 ~ 1, P26==1 & is.na(P27) ~ 2, P26==1 & !is.na(P27) ~ P27, is.na(P26) ~ P27, TRUE ~ P27), # Makeup remover  
    P173 = case_when(P172==0 ~ 1, P172==1 & is.na(P173) ~ 2, P172==1 & !is.na(P173) ~ P173, is.na(P172) ~ P173, TRUE ~ P173), # Perfume/cologne
    
    P30 = case_when(P29==0 ~ 1, P29==1 & is.na(P30) ~ 2, P29==1 & !is.na(P30) ~ P30, is.na(P29) ~ P30, TRUE ~ P30), # Cleansing cream 
    P33 = case_when(P32==0 ~ 1, P32==1 & is.na(P33) ~ 2, P32==1 & !is.na(P33) ~ P33, is.na(P32) ~ P33, TRUE ~ P33), # Face cream 
    P36 = case_when(P35==0 ~ 1, P35==1 & is.na(P36) ~ 2, P35==1 & !is.na(P36) ~ P36, is.na(P35) ~ P36, TRUE ~ P36), # Facial masks
    P39 = case_when(P38==0 ~ 1, P38==1 & is.na(P39) ~ 2, P38==1 & !is.na(P39) ~ P39, is.na(P38) ~ P39, TRUE ~ P39), # Anti-aging
    P41 = case_when(P40==0 ~ 1, P40==1 & is.na(P41) ~ 2, P40==1 & !is.na(P41) ~ P41, is.na(P40) ~ P41, TRUE ~ P41), # Spot lightener 
    P43 = case_when(P42==0 ~ 1, P42==1 & is.na(P43) ~ 2, P42==1 & !is.na(P43) ~ P43, is.na(P42) ~ P43, TRUE ~ P43), # Blemish/acne products
    P47 = case_when(P46==0 ~ 1, P46==1 & is.na(P47) ~ 2, P46==1 & !is.na(P47) ~ P47, is.na(P46) ~ P47, TRUE ~ P47), # Skin lighteners 
    P51 = case_when(P50==0 ~ 1, P50==1 & is.na(P51) ~ 2, P50==1 & !is.na(P51) ~ P51, is.na(P50) ~ P51, TRUE ~ P51), # Self-tanner
    P55 = case_when(P54==0 ~ 1, P54==1 & is.na(P55) ~ 2, P54==1 & !is.na(P55) ~ P55, is.na(P54) ~ P55, TRUE ~ P55), # Body lotion 
    P58 = case_when(P57==0 ~ 1, P57==1 & is.na(P58) ~ 2, P57==1 & !is.na(P58) ~ P58, is.na(P57) ~ P58, TRUE ~ P58), # Baby oil
    P61 = case_when(P60==0 ~ 1, P60==1 & is.na(P61) ~ 2, P60==1 & !is.na(P61) ~ P61, is.na(P60) ~ P61, TRUE ~ P61), # Petroleum jelly
    P64 = case_when(P63==0 ~ 1, P63==1 & is.na(P64) ~ 2, P63==1 & !is.na(P64) ~ P64, is.na(P63) ~ P64, TRUE ~ P64), # Hand lotions/creams
    P70 = case_when(P69==0 ~ 1, P69==1 & is.na(P70) ~ 2, P69==1 & !is.na(P70) ~ P70, is.na(P69) ~ P70, TRUE ~ P70), # Foot cream
    
    P73 = case_when(P72==0 ~ 1, P72==1 & is.na(P73) ~ 2, P72==1 & !is.na(P73) ~ P73, is.na(P72) ~ P73, TRUE ~ P73), # Bath/shower gel
    P76 = case_when(P75==0 ~ 1, P75==1 & is.na(P76) ~ 2, P75==1 & !is.na(P76) ~ P76, is.na(P75) ~ P76, TRUE ~ P76), # Deodorant 
    P81 = case_when(P80==0 ~ 1, P80==1 & is.na(P81) ~ 2, P80==1 & !is.na(P81) ~ P81, is.na(P80) ~ P81, TRUE ~ P81), # Talcum powder under arms 
    P85 = case_when(P84==0 ~ 1, P84==1 & is.na(P85) ~ 2, P84==1 & !is.na(P85) ~ P85, is.na(P84) ~ P85, TRUE ~ P85), # Talcum powder under vaginal area
    P89 = case_when(P88==0 ~ 1, P88==1 & is.na(P89) ~ 2, P88==1 & !is.na(P89) ~ P89, is.na(P88) ~ P89, TRUE ~ P89), # Talcum powder other areas
    P93 = case_when(P92==0 ~ 1, P92==1 & is.na(P93) ~ 2, P92==1 & !is.na(P93) ~ P93, is.na(P92) ~ P93, TRUE ~ P93), # Douch 
    P96 = case_when(P95==0 ~ 1, P95==1 & is.na(P96) ~ 2, P95==1 & !is.na(P96) ~ P96, is.na(P95) ~ P96, TRUE ~ P96), # Mouth wash 
    P182 = case_when(P181==0 ~ 1, P181==1 & is.na(P182) ~ 2, P181==1 & !is.na(P182) ~ P182, is.na(P181) ~ P182, TRUE ~ P182), # Shaving creams/gels
    
    P161 = case_when(P160==0 ~ 1, P160==1 & is.na(P161) ~ 2, P160==1 & !is.na(P161) ~ P161, is.na(P160) ~ P161, TRUE ~ P161), # Nail polish 
    P164 = case_when(P163==0 ~ 1, P163==1 & is.na(P164) ~ 2, P163==1 & !is.na(P164) ~ P164, is.na(P163) ~ P164, TRUE ~ P164), # Nail polish remover
    P167 = case_when(P166==0 ~ 1, P166==1 & is.na(P167) ~ 2, P166==1 & !is.na(P167) ~ P167, is.na(P166) ~ P167, TRUE ~ P167), # Nail artificial or fill in
    P170 = case_when(P169==0 ~ 1, P169==1 & is.na(P170) ~ 2, P169==1 & !is.na(P170) ~ P170, is.na(P169) ~ P170, TRUE ~ P170), 
    P67 = case_when(P66==0 ~ 1, P66==1 & is.na(P67) ~ 2, P66==1 & !is.na(P67) ~ P67, is.na(P66) ~ P67, TRUE ~ P67), # Cuticle cream
    
    # Hair (8)
    P99 = case_when(P98==0 ~ 1, P98==1 & is.na(P99) ~ 2, P98==1 & !is.na(P99) ~ P99, is.na(P98) ~ P99, TRUE ~ P99), # Shampoo
    P102 = case_when(P101==0 ~ 1, P101==1 & is.na(P102) ~ 2, P101==1 & !is.na(P102) ~ P102, is.na(P101) ~ P102, TRUE ~ P102), # Hair conditioner
    P106 = case_when(P105==0 ~ 1, P105==1 & is.na(P106) ~ 2, P105==1 & !is.na(P106) ~ P106, is.na(P105) ~ P106, TRUE ~ P106), # Hair spray 
    P109 = case_when(P108==0 ~ 1, P108==1 & is.na(P109) ~ 2, P108==1 & !is.na(P109) ~ P109, is.na(P108) ~ P109, TRUE ~ P109), # Hair styling gel 
    P112 = case_when(P111==0 ~ 1, P111==1 & is.na(P112) ~ 2, P111==1 & !is.na(P112) ~ P112, is.na(P111) ~ P112, TRUE ~ P112), # Pomade or hair grease
    P115 = case_when(P114==0 ~ 1, P114==1 & is.na(P115) ~ 2, P114==1 & !is.na(P115) ~ P115, is.na(P114) ~ P115, TRUE ~ P115), # Hair food 
    P155 = case_when(P154==0 ~ 1, P154==1 & is.na(P155) ~ 2, P154==1 & !is.na(P155) ~ P155, is.na(P154) ~ P155, TRUE ~ P155), # Minoxidil and Rogaine 
    P157 = case_when(P156==0 ~ 1, P156==1 & is.na(P157) ~ 2, P156==1 & !is.na(P157) ~ P157, is.na(P156) ~ P157, TRUE ~ P157)) # Pills such as Propecia for hair

mutate.var1 <- function(x)(ifelse(x==1, 1, 
                                  ifelse(x==2, 2, 
                                         ifelse(x==3, 3,
                                                ifelse(x %in% c(4,5), 4, 
                                                       ifelse(x==6, 5, x))))))

vangard1 = c("P2","P5","P9","P13","P16","P19","P23","P27","P173", # Makeup and Beauty (9)
             "P30","P33","P36","P39","P41","P43","P47","P51",
             "P58","P61","P55","P64","P70",# Skin care (13)
             "P76","P81","P85","P89","P93", "P96","P73","P182", # Hygiene (8)
             "P99","P102","P106","P109","P112","P115","P155","P157", # Hair (8)
             "P161","P164","P67","P167","P170", 
             "P129","P134","P121","P126","P118",
             "P137","P140","P143","P146","P149") # Nail (5)


# Delete several variables because non-professional application to others; 115 is pills for hair loss with low frequency
data3 = as_tibble(data2) %>%
  mutate_at(vangard1, mutate.var1) %>%
  mutate(mascara = ifelse(is.na(PC2), P2, PC2),
         eyeshadow = ifelse(is.na(PC4), P5, PC4), 
         eyeliner = ifelse(is.na(PC7), P9, PC7),
         lipstick = ifelse(is.na(PC10), P13, PC10),
         moisturizer = ifelse(is.na(PC12), P16, PC12),
         foundation = ifelse(is.na(PC14), P19, PC14),
         blush = ifelse(is.na(PC17), P23, PC17),
         makeupremover = ifelse(is.na(PC20), P27, PC20),
         
         perfume = ifelse(is.na(PC129), P173, PC129),  
         
         cleansing = ifelse(is.na(PC22), P30, PC22),
         facecream = ifelse(is.na(PC24), P33, PC24), 
         mask = ifelse(is.na(PC26), P36, PC26),
         antiaging = ifelse(is.na(PC27), P39, PC27),
         agespotlightener = ifelse(is.na(PC28), P41, PC28),
         blemish = ifelse(is.na(PC30), P43, PC30),
         skinlightener = ifelse(is.na(PC33), P47, PC33),
         tanner = ifelse(is.na(PC36), P51, PC36),
         babyoil = ifelse(is.na(PC39), P58, PC39),
         petroleumjelly = ifelse(is.na(PC41), P61, PC41),
         bodylotion = ifelse(is.na(PC43), P55, PC43), 
         handlotion = ifelse(is.na(PC45), P64, PC45),
         footcream = ifelse(is.na(PC47), P70, PC47),
         
         deodorant = ifelse(is.na(PC49), P76, PC49),
         mouthwash = ifelse(is.na(PC64), P96, PC64),
         bathgel = ifelse(is.na(PC66), P73, PC66),
         douche = ifelse(is.na(PC53), P93, PC53),
         talcarm = ifelse(is.na(PC55), P81, PC55),
         talcvaginal = ifelse(is.na(PC58), P85, PC58),
         talcother = ifelse(is.na(PC61), P89, PC61),
         shavingcream = ifelse(is.na(PC70), P182, PC70),
         
         shampoo = ifelse(is.na(PC72), P99, PC72),
         conditioner = ifelse(is.na(PC74), P102, PC74),
         hairspray = ifelse(is.na(PC77), P106, PC77),
         hairgel = ifelse(is.na(PC79), P109, PC79),
         pomade = ifelse(is.na(PC81), P112, PC81),
         hairfood = ifelse(is.na(PC83), P115, PC83),
         minoxidil = ifelse(is.na(PC114), P155, PC114),
         
         nailpolish = ifelse(is.na(PC119), P161, PC119),
         nailremover = ifelse(is.na(PC121), P164, PC121),
         cuticlecream = ifelse(is.na(PC123), P67, PC123),
         artificialnail = ifelse(is.na(PC125), P167, PC125))


# Product names and categories -------------
productname = c('mascara', 'eyeshadow', 'eyeliner', 'lipstick', 'moisturizer', 'foundation', 'blush', 'makeupremover', 'perfume', 'cleansing', 'facecream', 
                'mask', 'antiaging', 'agespotlightener', 'blemish', 'skinlightener', 'tanner', 'babyoil', 'petroleumjelly', 'bodylotion', 'handlotion', 
                'footcream', 'deodorant', 'mouthwash', 'bathgel', 'douche', 'talcarm', 'talcvaginal', 'talcother', 'shavingcream', 'shampoo', 'conditioner', 
                'hairspray', 'hairgel', 'pomade', 'hairfood', 'minoxidil', 'nailpolish', 'nailremover', 'cuticlecream','artificialnail')  


data4 = data3 %>%
  mutate(race = case_when(SE_RACE_ETH == 0 ~ 0, # non-Hispanic white
                          SE_RACE15 %in% c(3,4,11,12) ~ 1, # all Black, 
                          SE_RACE15 %in% c(2,6,8,10,14) ~ 2, # Hispanics
                          !is.na(SE_RACE15) ~ 3), 
         
         educ = case_when(SE18 %in% c(1,2,3,4,5) ~ 0,  # HS or less 
                          SE18 %in% c(6,7) ~ 1,    # some college
                          SE18 %in% c(8,9,10) ~ 2),  # college or higher 
         
         income = case_when(SE19Impute %in% c(1,2) ~ 0, # <50,000
                            SE19Impute %in% c(3) ~ 1,   # 50,000-<100,000
                            SE19Impute %in% c(4,5) ~ 2),  # >=100,000
         
         parity = case_when(PG_MedParity == 0 ~ 0, # 0
                            PG_MedParity == 1 ~ 1, # 1
                            PG_MedParity == 2 ~ 2, # 2 
                            PG_MedParity >= 3 ~ 3), # more
         
         parity_BC = case_when(parity %in% c(0,1) ~ 0, # because parity = 0 will be the same as nulliparous, so creating a variable for BC analysis
                               parity == 2 ~ 1, 
                               parity == 3 ~ 2),
         
         BMI = EX_BMI_final, 
         BMIcat = case_when(EX_BMI_CDC_final %in% c(1,2) ~ 0, 
                            EX_BMI_CDC_final == 3 ~ 1, 
                            EX_BMI_CDC_final %in% c(4,5,6) ~ 2), # <25, 25-30, >30 
         
         urban = case_when(RS18 %in% c(1) ~ 1, # Urban
                           RS18 %in% c(2,3,5) ~2, # Suburban, small town, other
                           RS18 %in% c(4) ~3), # Rural
         
         HEI = na_all_HEI2015, # got > 1147 missing 
         
         
         alcohol = case_when(AL_DrinkCat6 %in% c(0,1) ~ 0, # never and past 
                             AL_DrinkCat6 == 2 ~ 1, # current <1 drink  
                             AL_DrinkCat6 %in% c(3,4,5) ~ 2), # current >=1 drinks 
         
         agefirstbirth = case_when(PG_AgeFirstBirth<23 ~ 1, #<23
                                   PG_AgeFirstBirth>=23 & PG_AgeFirstBirth<27 ~ 2,#>=23, <27
                                   PG_AgeFirstBirth>=27 ~ 3, #>=27
                                   is.na(PG_AgeFirstBirth)&PG_Babies==0 ~ 0), # Nulliparous
         
         HRT_type = paste0(HR_HRTEstrOnly_Ever, HR_HRTProgOnly_Ever, HR_HRTEstrProg_Ever),
         
         # HRT for breast and ovarian cancer analyses
         HRT1 = case_when(HRT_type=="000"~0,
                          HRT_type %in% c("001","010","011","110","101","111")~2,# Estrogen plus progestin   
                          HRT_type %in% c("100")~1), # Estrogen only 
         
         # HRT2 for uterine cancer analyses
         HRT2 = case_when(HRT_type=="000"~0,
                          HRT_type %in% c("001","010","011")~2,# Estrogen plus progestin 
                          HRT_type %in% c("100","101","111","110")~1),# Estrogen only 
         
         BCduration = case_when(HR_BCpill_Years == 0 | HR_BCpill_Ever == 0 ~ 0, # None
                                0 < HR_BCpill_Years & HR_BCpill_Years < 2 ~ 1, # <2
                                10 > HR_BCpill_Years & HR_BCpill_Years >= 2 ~ 2, # >=2-10
                                HR_BCpill_Years >= 10 ~ 3), # >=10 
         
         smoking = ifelse(SM_SmokeStatusN %in% c(1,2), 1, SM_SmokeStatusN), # past or current, never 
         
         menarche = case_when(PG_MenarcheAge>0 & PG_MenarcheAge<12 ~ 1, 
                              PG_MenarcheAge>=12 ~ 0),
         PA = PH_CurrentTotMETHrsPerWeek, 
         menopause = HR_Menopause_T0,  # menopausal status at baseline with less missing 
         ADI = P_ADI2000_NatRank, 
         breastfeed = ifelse(PG_BreastTotal>=48, 1, 0), # >= 48 (4*12months) weeks #Similar missing at t0 and t4

         # menopuasual age: use whatever information is available
         menoage_NA = paste0(str_sub(!is.na(HR_MenopauseAgeExact),star=1,end=1), 
                             str_sub(!is.na(HR_MenopauseAgeExact_T0),star=1,end=1), 
                             str_sub(!is.na(HR_MenopauseAgeExact_T1),star=1,end=1), 
                             str_sub(!is.na(HR_MenopauseAgeExact_T2),star=1,end=1), 
                             str_sub(!is.na(HR_MenopauseAgeExact_T3),star=1,end=1), 
                             str_sub(!is.na(HR_MenopauseAgeExact_T4),star=1,end=1), 
                             str_sub(!is.na(FU_BCInvD_MenopauseAgeExact),star=1,end=1)), 
         # make sure all the possible values were hormonized in the same menoage variable
         menoage = case_when(str_sub(menoage_NA, start=7, end=7)=="T" ~ FU_BCInvD_MenopauseAgeExact,
                             menoage_NA %in% c("FFFFFTF", "FFFFTTF","FFFTTTF","FFTTTTF","TTTTTTF") ~ HR_MenopauseAgeExact_T4, 
                             menoage_NA %in% c("FFFFTFF", "FFFTTFF","FFTTTFF","TTTTTFF") ~ HR_MenopauseAgeExact_T3, 
                             menoage_NA %in% c("FFFTFFF", "FFTTFFF", "TTTTFFF") ~ HR_MenopauseAgeExact_T2,
                             menoage_NA %in% c("FFTFFFF") ~ HR_MenopauseAgeExact_T1,
                             TRUE ~ NA_real_))

# Already tried to impute menoage using HR_MenopauseStatus and the age at baseline and follow-up, this approach will not reduce the number of missing
data4%>%select(menoage)%>%skimr::skim() #m issingness of menoage 3145

meno.yn <- function(x)(ifelse(x %in% c(6,7,9,11), 0,  # premenopause 
                              ifelse(x %in% c(1,2,3,4,5,10), 1, NA))) # postmenopause
library(stringi)
data4 %>% filter(HR_MenopauseStatus!=HR_MenopauseStatus_T0) %>% select(HR_MenopauseStatus,HR_MenopauseStatus_T0, HR_MenopauseStatus_T1) # HR_MenopauseStatus and HR_MenopauseStatus_T0 are very similar, only 5 difference from unknown to known
data5 = data4 %>%
  mutate_at(c("HR_MenopauseStatus", "HR_MenopauseStatus_T0", "HR_MenopauseStatus_T1", "HR_MenopauseStatus_T2", "HR_MenopauseStatus_T3", "HR_MenopauseStatus_T4"), funs(yn=meno.yn(.)))%>% 
  # change NA to 9 
  mutate_at(c("HR_MenopauseStatus_yn", "HR_MenopauseStatus_T0_yn", "HR_MenopauseStatus_T1_yn", "HR_MenopauseStatus_T2_yn", "HR_MenopauseStatus_T3_yn", "HR_MenopauseStatus_T4_yn"),~ coalesce(.,9))%>%
  # All the menopause status at baseline/follow-up 
  mutate(HZ_MenopauseStatus = paste0(HR_MenopauseStatus_yn, HR_MenopauseStatus_T0_yn, HR_MenopauseStatus_T1_yn, HR_MenopauseStatus_T2_yn, HR_MenopauseStatus_T3_yn, HR_MenopauseStatus_T4_yn), 
         # First time report postmenopausal status 
         firstpost_loc = str_locate(HZ_MenopauseStatus,"1")[,1], 
         # Last time report premenopausal status 
         lastpre_loc = stri_locate_last_fixed(HZ_MenopauseStatus,"0")[,1],
         
         baseline_meno = menopause, 
         eof_meno = FU_BCInvD_Menopause,
         # also make sure that the information from the menoage was used (imputed one missing)
         eof_meno = ifelse(is.na(eof_meno) & menoage<=FU_BCInvD_EOFAgeExact, 1, eof_meno),
         # resolve disagreement using menoage 
         eof_meno = ifelse(!is.na(menoage)& menoage<=FU_BCInvD_EOFAgeExact, 1, eof_meno), 
         eof_meno = ifelse(!is.na(menoage)& menoage>FU_BCInvD_EOFAgeExact, 0, eof_meno), 
         # if EOF age below 55 years old, impute as premenopausal 
         eof_meno = ifelse(is.na(eof_meno)& FU_BCInvD_EOFAgeExact<55, 0, eof_meno))

write_csv(data5, "clean_data/all.csv")

# Before excluding participants based on each cancer outcomes; excluded the participants without any answer to PCP questionnaire 
n_for_text = data5 %>% filter_at(vars(all_of(productname)), any_vars(!is.na(.)))
dim(n_for_text) 

# Dataset for breast cancer ------------
table(na_tag(data5$FU_BCInvD_Event))
# withdrew 4
# pre-baseline diagnosis 59 
# uncertain diagnosis 5
# unknown timing (relative to baseline) 17

dataBC = data5 %>%
  filter(!is.na(FU_BCInvD_Event)) 

# Create ER-/+
dataBC = dataBC %>%
  mutate(BCeventERpos = case_when(FU_BCInvD_DxER_Result == 1 ~ 1, 
                                  FU_BCInvD_DxER_Result %in% c(2,3) ~ 0, # FU_BCInvD_DxER_Result == 3 --> borderline; consider NOT the type
                                  is.na(FU_BCInvD_DxER_Result) | FU_BCInvD_Event == 0 ~ 0),
         BCeventERneg = case_when(FU_BCInvD_DxER_Result == 2 ~ 1, 
                                  FU_BCInvD_DxER_Result %in% c(1,3) ~ 0, # FU_BCInvD_DxER_Result == 3 --> borderline 
                                  is.na(FU_BCInvD_DxER_Result) | FU_BCInvD_Event == 0 ~ 0),
         BCeventInv = FU_BCInvNoD_Event, 
         BCeventD = case_when(FU_BCInvNoD_Event==0 & FU_BCInvD_Event==1 ~ 1, 
                              TRUE ~ 0),
         FU_BCInvD_Event_MR = ifelse(FU_BCInvD_Event == 1 & FU_BCInvD_DxType_Source %in% c(6,9), 0, FU_BCInvD_Event))

# Remove no contribution to person-time
dataBC = dataBC %>%
  filter(FU_BCInvD_EOFAgeExact>AgeExact_Baseline) %>% 
  filter_at(vars(all_of(productname)), any_vars(!is.na(.))) 

write_csv(dataBC, "clean_data/dataBC.csv")

# Dataset for ovarian cancer --------------- 
# NEED TO CHANGE the EOF age # check 
table(na_tag(data5$FU_OvCa_Event))
# withdrew 4
# pre-baseline diagnosis 204 
# uncertain diagnosis 13
# unknown timing (relative to baseline) 30

dataOv = data5 %>%
  filter(!is.na(FU_OvCa_Event)) %>% # n = 50884-50633 (rm 251)
  mutate(HR_OOPHAGEimp = ifelse(!is.na(HR_OOPHAGEExact), HR_OOPHAGEExact, HR_OOPHAGE), 
         HR_OOPHAgeimp_T1 = ifelse(!is.na(HR_OOPHAgeExact_T1), HR_OOPHAgeExact_T1, HR_OOPHAge_T1),
         HR_OOPHAgeimp_T2 = ifelse(!is.na(HR_OOPHAgeExact_T2), HR_OOPHAgeExact_T2, HR_OOPHAge_T2),
         HR_OOPHAgeimp_T3 = ifelse(!is.na(HR_OOPHAgeExact_T3), HR_OOPHAgeExact_T3, HR_OOPHAge_T3),
         HR_OOPHAgeimp_T4 = ifelse(!is.na(HR_OOPHAgeExact_T4), HR_OOPHAgeExact_T4, HR_OOPHAge_T4))

# HR_OOPH=0, no; =1, one side; =2 both; =3 unknown side; =4 partial. 
# Use follow up data for oophorectomy
dataOv = dataOv %>%
  mutate_at(c("HR_OOPH", "HR_OOPH_T1","HR_OOPH_T2","HR_OOPH_T3","HR_OOPH_T4"), ~coalesce(.,9))%>% # missing as 9
  mutate(HZ_OOPH = paste0(HR_OOPH, HR_OOPH_T1,HR_OOPH_T2,HR_OOPH_T3,HR_OOPH_T4),
         HZ_OOPH2 = ifelse(str_detect(HZ_OOPH,"2"),1,0),
         OOPHage_loc = str_locate(HZ_OOPH,"2")[,1])

dataOv <- dataOv %>% mutate(OOPHage = case_when(
  # Use age at follow up for missing age at bilateral oophorectomy 
  OOPHage_loc==1 & !is.na(HR_OOPHAGEimp) ~ HR_OOPHAGEimp,
  OOPHage_loc==1 & is.na(HR_OOPHAGEimp) & !is.na(AgeExact_Baseline)~ AgeExact_Baseline,
  OOPHage_loc==1 & is.na(HR_OOPHAGEimp) & is.na(AgeExact_Baseline) ~  NA_real_,
  
  OOPHage_loc==2 & !is.na(HR_OOPHAgeimp_T1) ~ HR_OOPHAgeimp_T1,
  OOPHage_loc==2 & is.na(HR_OOPHAgeimp_T1) & !is.na(AgeExact_T1)~ AgeExact_T1,
  OOPHage_loc==2 & is.na(HR_OOPHAgeimp_T1) & is.na(AgeExact_T1) ~  NA_real_,
  
  OOPHage_loc==3 & !is.na(HR_OOPHAgeimp_T2) ~ HR_OOPHAgeimp_T2,
  OOPHage_loc==3 & is.na(HR_OOPHAgeimp_T2) & !is.na(AgeExact_T2)~ AgeExact_T2,
  OOPHage_loc==3 & is.na(HR_OOPHAgeimp_T2) & is.na(AgeExact_T2) ~  NA_real_,
  
  OOPHage_loc==4 & !is.na(HR_OOPHAgeimp_T3) ~ HR_OOPHAgeimp_T3,
  OOPHage_loc==4 & is.na(HR_OOPHAgeimp_T3) & !is.na(AgeExact_T3)~ AgeExact_T3,
  OOPHage_loc==4 & is.na(HR_OOPHAgeimp_T3) & is.na(AgeExact_T3) ~  NA_real_,
  
  OOPHage_loc==5 & !is.na(HR_OOPHAgeimp_T4) ~ HR_OOPHAgeimp_T4,
  OOPHage_loc==5 & is.na(HR_OOPHAgeimp_T4) & !is.na(AgeExact_T4)~ AgeExact_T4,
  OOPHage_loc==5 & is.na(HR_OOPHAgeimp_T4) & is.na(AgeExact_T4) ~  NA_real_,
  
  is.na(OOPHage_loc)~ NA_real_))

ooph_list <- dataOv %>% filter(HZ_OOPH2==1 & OOPHage<=AgeExact_Baseline) %>% select(PSID)
save(ooph_list , file = "output/ooph_list.RData")

dataOv = dataOv %>%
  filter(!(HZ_OOPH2==1 & OOPHage<=AgeExact_Baseline)) 


dataOv %>% filter(HZ_OOPH2==1 &  FU_OvCa_Event==1 & OOPHage<FU_OvCa_EOFAgeExact) %>% 
  select(PSID, HZ_OOPH, OOPHage_loc, AgeExact_Baseline, FU_OvCa_EOFAgeExact, OOPHage, FU_OvCa_DxType_Source) %>% select(PSID)
# 4 people had a bilateral oophorectomy before cancer, keep them as cases since their age at ooph and cancer are closed and mostly reported by medical record  
# 1) Medical Report n=3
# 9) Next of kin n=1

dataOv <- dataOv %>%
  # censor those who have a bilateral oophorectomy
  mutate(FU_OvCa_EOFAgeExact_ooph = ifelse(HZ_OOPH2==1 & OOPHage<FU_OvCa_EOFAgeExact, OOPHage, FU_OvCa_EOFAgeExact),
         FU_OvCa_EOFAgeExact_ooph = ifelse(PSID %in% c("00287_203493","00287_213678","00287_222562","00287_225851"), FU_OvCa_EOFAgeExact, FU_OvCa_EOFAgeExact_ooph),
         
         # medically confirmed cases 
         FU_OvCa_Event_MR = ifelse(FU_OvCa_Event == 1 & FU_OvCa_DxType_Source %in% c(6,9), 0, FU_OvCa_Event), 
         
         # endometrial
         OveventSerous = ifelse(FU_OvCa_Event_MR == 1 & FU_OvCa_DxHist01 %in% c(8020, 8021, 8022, 8050, 8120, 8130, 8260, 8441, 8442, 8450, 8460, 8461, 8462, 8463, 9014), 1, 0), 
         OveventNonserous = ifelse(FU_OvCa_Event_MR == 1 & !FU_OvCa_DxHist01 %in% c(8020, 8021, 8022, 8050, 8120, 8130, 8260, 8441, 8442, 8450, 8460, 8461, 8462, 8463, 9014) , 1, 0))

# Remove no contribution to person-time
dataOv = dataOv %>%
  filter(FU_OvCa_EOFAgeExact_ooph>AgeExact_Baseline) %>% 
  filter_at(vars(all_of(productname)), any_vars(!is.na(.))) 

write_csv(dataOv, "clean_data/dataOv.csv")


# Dataset for uterine cancer ---------------
table(na_tag(data5$FU_UECa_Event))
# withdrew 4
# pre-baseline diagnosis 381
# uncertain diagnosis 12
# unknown timing (relative to baseline) 55

dataUE = data5 %>%
  filter(!is.na(FU_UECa_Event)) %>% # n = 50884-50432 (rm 251)
  mutate_at(c("HR34", "HR_Hyst_T1","HR_Hyst_T2","HR_Hyst_T3","HR_Hyst_T4"), ~ coalesce(.,9))%>% # transform missing to 9 
  mutate(HZ_hyst = paste0(HR34, HR_Hyst_T1, HR_Hyst_T2, HR_Hyst_T3, HR_Hyst_T4),
         # if had a hysterectomy, HZ_Hyst_yn=1, otherwise = 0 including na
         HZ_hyst_yn = ifelse(str_detect(HZ_hyst,"1"),1,0), 
         hyst_loc = str_locate(HZ_hyst,"1")[,1],
         
         # impute hyst age at baseline and follow-up
         hystage_bl = ifelse(!is.na(HR35Exact), HR35Exact, HR35),
         hystage_t1 = ifelse(!is.na(HR_HystAgeExact_T1), HR_HystAgeExact_T1, HR_HystAge_T1), 
         hystage_t2 = ifelse(!is.na(HR_HystAgeExact_T2), HR_HystAgeExact_T2, HR_HystAge_T2),
         hystage_t3 = ifelse(!is.na(HR_HystAgeExact_T3), HR_HystAgeExact_T3, HR_HystAge_T3),
         hystage_t4 = ifelse(!is.na(HR_HystAgeExact_T4), HR_HystAgeExact_T4, HR_HystAge_T4), 
         
         na_age = paste0(ifelse(is.na(hystage_bl),1,0), ifelse(is.na(hystage_t1),1,0),ifelse(is.na(hystage_t2),1,0), ifelse(is.na(hystage_t3),1,0),ifelse(is.na(hystage_t4),1,0)),
         
         # prioritize the age to a recent followup
         hystage = hystage_t4, 
         hystage = ifelse(na_age=="00001", hystage_t3, hystage), 
         hystage = ifelse(na_age=="00011", hystage_t2, hystage), 
         hystage = ifelse(na_age=="00111", hystage_t1, hystage), 
         hystage = ifelse(na_age=="01111", hystage_bl, hystage), 
         hystage = ifelse(na_age=="10001", hystage_t3, hystage), 
         hystage = ifelse(na_age=="10011", hystage_t2, hystage), 
         hystage = ifelse(na_age=="10111", hystage_t1, hystage),
         hystage = ifelse(na_age=="11001", hystage_t3, hystage), 
         hystage = ifelse(na_age=="11011", hystage_t2, hystage), 
         hystage = ifelse(na_age=="11101", hystage_t3, hystage),
         
         # Impute missing age with age at follow-up
         hystage = case_when(is.na(hystage) & HZ_hyst_yn==1 & hyst_loc==5 ~ AgeExact_T4, 
                             is.na(hystage) & HZ_hyst_yn==1 & hyst_loc==4 ~ AgeExact_T3,
                             is.na(hystage) & HZ_hyst_yn==1 & hyst_loc==3 ~ AgeExact_T2, 
                             is.na(hystage) & HZ_hyst_yn==1 & hyst_loc==2 ~ AgeExact_T1, 
                             is.na(hystage) & HZ_hyst_yn==1 & hyst_loc==1 ~ AgeExact_Baseline, 
                             TRUE~hystage)) 

hyst_list <- dataUE %>% filter(HZ_hyst_yn==1&hystage<=AgeExact_Baseline) %>% select(PSID)
save(hyst_list , file = "output/hyst_list.RData")

dataUE <- dataUE %>% filter(!(HZ_hyst_yn==1&hystage<=AgeExact_Baseline))# n = 50432-34833 (rm 15599)

dataUE %>% filter(HZ_hyst_yn==1 & FU_UECa_Event==1 & hystage<FU_UECa_EOFAgeExact) %>% select(PSID, hystage, FU_UECa_EOFAgeExact) # one subject who have hysterectomy BEFORE cancer diagnosis
dataUE %>% filter(PSID=="00287_213678") %>% select(FU_UECa_DxType_Source) #  it is a medically confirmed case; keep this as a case (event_hyst = event; age_eof_hyst = hystage) 

dataUE <- dataUE %>% 
  mutate(FU_UECa_EOFAgeExact_hyst = ifelse(HZ_hyst_yn==1 & hystage<FU_UECa_EOFAgeExact, hystage, FU_UECa_EOFAgeExact),
         FU_UECa_EOFAgeExact_hyst = ifelse(PSID=="00298_233614", FU_UECa_EOFAgeExact, FU_UECa_EOFAgeExact_hyst),
         
         # medically confirmed cases 
         FU_UECa_Event_MR = ifelse(FU_UECa_Event == 1 & FU_UECa_DxType_Source %in% c(6,9), 0, FU_UECa_Event), 
                            
         # endometrial
         UEeventType1 = ifelse(FU_UECa_Event_MR == 1 & FU_UECa_DxHist01 %in% c(8140,8262,8380,8382,8480,8560,8570), 1, 0), 
         UEeventType2 = ifelse(FU_UECa_Event_MR == 1 & FU_UECa_DxHist01 %in% c(8310,8323,8441,8460,8950,8980), 1, 0),
         
         # Assuming the subjects with endometrial cancer related histology code are also endometrial cancer
         UEeventEM = ifelse(FU_UECa_Event_MR==1 & (FU_UECa_DxType == 11.2| FU_UECa_DxHist01 %in% c(8140,8262,8380,8382,8480,8560,8570,8310,8323,8441,8460,8950,8980)), 1, 0))

# Remove no contribution to person-time
dataUE = dataUE %>% filter(FU_UECa_EOFAgeExact_hyst>AgeExact_Baseline) %>% # n = 34833-34650 (rm 183) 
  filter_at(vars(all_of(productname)), any_vars(!is.na(.))) #n = 34650-33976 (rm 674)

write_csv(dataUE, "clean_data/dataUE.csv")

# 
dataOv %>% filter(OOPHage<FU_OvCa_EOFAgeExact_ooph & FU_OvCa_Event==1) %>% select(PSID, AgeExact_Baseline, OOPHage, FU_OvCa_EOFAgeExact_ooph) %>% mutate(diff=FU_OvCa_EOFAgeExact_ooph-OOPHage)
dataUE %>% filter(HZ_hyst_yn==1 & hystage<FU_UECa_EOFAgeExact_hyst & FU_UECa_Event==1) 


# Breast cancer pre-/post- menopause  --------------------- 
dataBC_meno <- dataBC %>%
  mutate(postmeno_time = case_when(baseline_meno == 0 & eof_meno == 0 ~ 0, 
                                   baseline_meno == 0 & eof_meno == 1 ~ 1, 
                                   baseline_meno == 1 ~ 1, 
                                   TRUE ~ NA_real_))


dataBC_meno$eof_meno%>%skimr::skim() # missing n=8
dataBC_meno$baseline_meno%>%skimr::skim() # missing n=7
dataBC_meno$menoage%>%skimr::skim() # missing n=2895

pre <- dataBC_meno %>%
  mutate(tstart = case_when(baseline_meno == 0 & eof_meno == 0 ~ AgeExact_Baseline, 
                            baseline_meno == 0 & eof_meno == 1 ~ AgeExact_Baseline, 
                            TRUE ~ NA_real_),
         tstop = case_when(baseline_meno == 0 & eof_meno == 0 ~ FU_BCInvD_EOFAgeExact, 
                           baseline_meno == 0 & eof_meno == 1  ~ menoage,
                           TRUE ~ NA_real_),
         event3 = ifelse(FU_BCInvD_Event == 1 & postmeno_time == 0, 1, 0),
         prepost = 0)

post <- dataBC_meno %>%
  mutate(tstart = case_when(baseline_meno == 0 & eof_meno == 0 ~ NA_real_, 
                            baseline_meno == 0 & eof_meno == 1 ~ menoage, 
                            baseline_meno == 1 ~ AgeExact_Baseline, 
                            TRUE~NA_real_), 
         tstop = case_when(baseline_meno == 0 & eof_meno == 0 ~ NA_real_, 
                           baseline_meno == 0 & eof_meno == 1  ~ FU_BCInvD_EOFAgeExact,
                           baseline_meno == 1 ~ FU_BCInvD_EOFAgeExact, 
                           TRUE~NA_real_), 
         event3 = ifelse(FU_BCInvD_Event == 1 & postmeno_time == 1, 1, 0),
         prepost = 1)

newdat <- rbind(pre,post) %>% filter(!is.na(tstart))
write_csv(newdat, "clean_data/BCmeno.csv")

dataBC_pre <- newdat %>% as_tibble() %>% filter(prepost==0)
dataBC_post <- newdat %>% as_tibble() %>% filter(prepost==1)

dataBC_pre %>% filter(event3==1, is.na(eof_meno)) 
dataBC_post %>% filter(event3==1, is.na(eof_meno)) 

write_csv(dataBC_pre , "clean_data/BCpre.csv")
write_csv(dataBC_post, "clean_data/BCpost.csv")


# Ovarian cancer pre-/post- menopause  --------------------- 
dataOv_meno <- dataOv %>%
  mutate(eof_meno2 = case_when(
    # When having the same age at EOF, use the same menostatus 
    baseline_meno==0 & menoage > FU_OvCa_EOFAgeExact_ooph ~ 0, 
    baseline_meno==0 & menoage <= FU_OvCa_EOFAgeExact_ooph ~ 1,
    TRUE~eof_meno),
    
    eof_meno2 = case_when(   
      # Age at first time reported postmenopausal <= EOF age -> postmenopausal  
      is.na(eof_meno2) & firstpost_loc==1 & FU_OvCa_EOFAgeExact_ooph>= AgeExact_Baseline ~ 1, 
      is.na(eof_meno2) & firstpost_loc==2 & FU_OvCa_EOFAgeExact_ooph>= AgeExact_T0 ~ 1,
      is.na(eof_meno2) & firstpost_loc==3 & FU_OvCa_EOFAgeExact_ooph>= AgeExact_T1 ~ 1,
      is.na(eof_meno2) & firstpost_loc==4 & FU_OvCa_EOFAgeExact_ooph>= AgeExact_T2 ~ 1,
      is.na(eof_meno2) & firstpost_loc==5 & FU_OvCa_EOFAgeExact_ooph>= AgeExact_T3 ~ 1,
      is.na(eof_meno2) & firstpost_loc==6 & FU_OvCa_EOFAgeExact_ooph>= AgeExact_T4 ~ 1,
      
      # Age at last time reported premenopausal >= EOF age -> premenopausal 
      is.na(eof_meno2) & lastpre_loc==1 & FU_OvCa_EOFAgeExact_ooph< AgeExact_Baseline ~ 0, 
      is.na(eof_meno2) & lastpre_loc==2 & FU_OvCa_EOFAgeExact_ooph< AgeExact_T0 ~ 0,
      is.na(eof_meno2) & lastpre_loc==3 & FU_OvCa_EOFAgeExact_ooph< AgeExact_T1 ~ 0,
      is.na(eof_meno2) & lastpre_loc==4 & FU_OvCa_EOFAgeExact_ooph< AgeExact_T2 ~ 0,
      is.na(eof_meno2) & lastpre_loc==5 & FU_OvCa_EOFAgeExact_ooph< AgeExact_T3 ~ 0,
      is.na(eof_meno2) & lastpre_loc==6 & FU_OvCa_EOFAgeExact_ooph< AgeExact_T4 ~ 0,
      
      TRUE~eof_meno2), 
    # if EOF age below 55 years old, impute as premenopausal 
    eof_meno2 = ifelse(is.na(eof_meno2)&FU_OvCa_EOFAgeExact_ooph<55, 0, eof_meno2))

dataOv_meno %>% select(eof_meno2) %>% skimr::skim() # missing 5      

dataOv_meno <- dataOv_meno %>% 
  mutate(postmeno_time = case_when(baseline_meno == 0 & eof_meno2 == 0 ~ 0, 
                                   baseline_meno == 0 & eof_meno2 == 1 ~ 1, 
                                   baseline_meno == 1 ~ 1, 
                                   TRUE ~ NA_real_))

pre <- dataOv_meno %>%
  mutate(tstart = case_when(baseline_meno == 0 & eof_meno2 == 0 ~ AgeExact_Baseline, 
                            baseline_meno == 0 & eof_meno2 == 1 ~ AgeExact_Baseline, 
                            TRUE ~ NA_real_),
         tstop = case_when(baseline_meno == 0 & eof_meno2 == 0 ~ FU_OvCa_EOFAgeExact_ooph, 
                           baseline_meno == 0 & eof_meno2 == 1  ~ menoage,
                           TRUE ~ NA_real_),
         event3 = ifelse(FU_OvCa_Event == 1 & postmeno_time == 0, 1, 0),
         prepost = 0)

post <- dataOv_meno %>%
  mutate(tstart = case_when(baseline_meno == 0 & eof_meno2 == 0 ~ NA_real_, 
                            baseline_meno == 0 & eof_meno2 == 1 ~ menoage, 
                            baseline_meno == 1 ~ AgeExact_Baseline, 
                            TRUE~NA_real_), 
         tstop = case_when(baseline_meno == 0 & eof_meno2 == 0 ~ NA_real_, 
                           baseline_meno == 0 & eof_meno2 == 1  ~ FU_OvCa_EOFAgeExact_ooph,
                           baseline_meno == 1 ~ FU_OvCa_EOFAgeExact_ooph, 
                           TRUE~NA_real_), 
         event3 = ifelse(FU_OvCa_Event == 1 & postmeno_time == 1, 1, 0),
         prepost = 1)


newdat <- rbind(pre,post) %>% filter(!is.na(tstart))
table(newdat$prepost, useNA='ifany')

write_csv(newdat, "clean_data/Ovmeno.csv")

dataOv_pre <- newdat %>% as_tibble() %>% filter(prepost==0)
dataOv_post <- newdat %>% as_tibble() %>% filter(prepost==1)

write_csv(dataOv_pre , "clean_data/Ovpre.csv")
write_csv(dataOv_post, "clean_data/Ovpost.csv")

# Uterine cancer pre-/post- menopause  --------------------- 
dataUE_meno <- dataUE %>%
  mutate(eof_meno2 = case_when(
    # When having the same age at EOF, use the same menostatus 
    baseline_meno==0 & menoage > FU_UECa_EOFAgeExact_hyst ~ 0, 
    baseline_meno==0 & menoage <= FU_UECa_EOFAgeExact_hyst ~ 1,
    TRUE~eof_meno),
    
    eof_meno2 = case_when(   
      # Age at first time reported postmenopausal <= EOF age -> postmenopausal  
      is.na(eof_meno2) & firstpost_loc==1 & FU_UECa_EOFAgeExact_hyst>= AgeExact_Baseline ~ 1, 
      is.na(eof_meno2) & firstpost_loc==2 & FU_UECa_EOFAgeExact_hyst>= AgeExact_T0 ~ 1,
      is.na(eof_meno2) & firstpost_loc==3 & FU_UECa_EOFAgeExact_hyst>= AgeExact_T1 ~ 1,
      is.na(eof_meno2) & firstpost_loc==4 & FU_UECa_EOFAgeExact_hyst>= AgeExact_T2 ~ 1,
      is.na(eof_meno2) & firstpost_loc==5 & FU_UECa_EOFAgeExact_hyst>= AgeExact_T3 ~ 1,
      is.na(eof_meno2) & firstpost_loc==6 & FU_UECa_EOFAgeExact_hyst>= AgeExact_T4 ~ 1,
      
      # Age at last time reported premenopausal > EOF age -> premenopausal 
      is.na(eof_meno2) & lastpre_loc==1 & FU_UECa_EOFAgeExact_hyst< AgeExact_Baseline ~ 0, 
      is.na(eof_meno2) & lastpre_loc==2 & FU_UECa_EOFAgeExact_hyst< AgeExact_T0 ~ 0,
      is.na(eof_meno2) & lastpre_loc==3 & FU_UECa_EOFAgeExact_hyst< AgeExact_T1 ~ 0,
      is.na(eof_meno2) & lastpre_loc==4 & FU_UECa_EOFAgeExact_hyst< AgeExact_T2 ~ 0,
      is.na(eof_meno2) & lastpre_loc==5 & FU_UECa_EOFAgeExact_hyst< AgeExact_T3 ~ 0,
      is.na(eof_meno2) & lastpre_loc==6 & FU_UECa_EOFAgeExact_hyst< AgeExact_T4 ~ 0,
      
      TRUE~eof_meno2), 
    # if EOF age below 55 years old, impute as premenopausal 
    eof_meno2 = ifelse(is.na(eof_meno2)&FU_UECa_EOFAgeExact_hyst<55, 0, eof_meno2))


dataUE_meno <- dataUE_meno %>% 
  mutate(postmeno_time = case_when(baseline_meno == 0 & eof_meno2 == 0 ~ 0, 
                                   baseline_meno == 0 & eof_meno2 == 1 ~ 1, 
                                   baseline_meno == 1 ~ 1, 
                                   TRUE ~ NA_real_))

pre <- dataUE_meno %>%
  mutate(tstart = case_when(baseline_meno == 0 & eof_meno2 == 0 ~ AgeExact_Baseline, 
                            baseline_meno == 0 & eof_meno2 == 1 ~ AgeExact_Baseline, 
                            TRUE ~ NA_real_),
         tstop = case_when(baseline_meno == 0 & eof_meno2 == 0 ~ FU_UECa_EOFAgeExact_hyst, 
                           baseline_meno == 0 & eof_meno2 == 1  ~ menoage,
                           TRUE ~ NA_real_),
         event3 = ifelse(FU_UECa_Event == 1 & postmeno_time == 0, 1, 0),
         prepost = 0)

post<- dataUE_meno %>%
  mutate(tstart = case_when(baseline_meno == 0 & eof_meno2 == 0 ~ NA_real_, 
                            baseline_meno == 0 & eof_meno2 == 1 ~ menoage, 
                            baseline_meno == 1 ~ AgeExact_Baseline, 
                            TRUE~NA_real_), 
         tstop = case_when(baseline_meno == 0 & eof_meno2 == 0 ~ NA_real_, 
                           baseline_meno == 0 & eof_meno2 == 1  ~ FU_UECa_EOFAgeExact_hyst,
                           baseline_meno == 1 ~ FU_UECa_EOFAgeExact_hyst,
                           TRUE~NA_real_), 
         event3 = ifelse(FU_UECa_Event == 1 & postmeno_time == 1, 1, 0),
         prepost = 1)

newdat <- rbind(pre,post) %>% filter(!is.na(tstart))
write_csv(newdat, "clean_data/UEmeno.csv")

dataUE_pre <- newdat %>% as_tibble() %>% filter(prepost==0)
dataUE_post <- newdat %>% as_tibble() %>% filter(prepost==1)

write_csv(dataUE_pre , "clean_data/UEpre.csv")
write_csv(dataUE_post, "clean_data/UEpost.csv")




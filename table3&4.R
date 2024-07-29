# Table 3 & 4 


library(tidyverse)
library(survival) # coxph
library(survminer) # coxph
library(stringr)
library(reshape2)
library(qgcomp)
library(naniar)
library(rms)
library(lmtest)
library(qgcompint)

# Load data ---
dataBC <- read_csv("clean_data/dataBC.csv") 
dataOv <- read_csv("clean_data/dataOv.csv") 
dataUE <- read_csv("clean_data/dataUE.csv") 

covs1 <- c("race","educ", "income","menopause", "smoking", "alcohol","BCduration", "HRT1", "PA", "BMI")
covs2 <- c("race","educ", "income","menopause", "smoking", "alcohol","BCduration", "HRT2", "PA", "BMI")
productname = c('blush', 'eyeliner', 'eyeshadow', 'foundation', 'lipstick', 'mascara', 'makeupremover', 
                'perfume', 'artificialnail', 'cuticlecream', 'nailpolish', 'nailremover', 
                'bathgel', 'deodorant', 'douche', 'mouthwash', 'shavingcream', 'talcarm', 'talcvaginal', 'talcother',    
                'antiaging', 'agespotlightener', 'babyoil', 'blemish', 'bodylotion', 'cleansing', 'facecream', 'mask', 
                'footcream', 'handlotion', 'moisturizer','petroleumjelly', 'skinlightener', 'tanner')  

# Breast Cancer ---
dat1 <- dataBC %>%
  filter_at(vars(all_of(covs1),  "AgeExact_Baseline", "FU_BCInvD_EOFAgeExact", "FU_BCInvD_Event"), all_vars(!is.na(.)))%>%
  mutate_at(vars("race","educ","income", "menopause", "smoking", "alcohol", "BCduration", "HRT1"), factor)%>% 
  mutate(BMI_3c = case_when(BMI<25 ~ 0, 
                            BMI>=25 & BMI<30 ~ 1, 
                            BMI>=30 ~ 2))

# Ovarian Cancer ---
dat2 <- dataOv %>%
  filter_at(vars(all_of(covs1), "AgeExact_Baseline", "FU_OvCa_EOFAgeExact_ooph", "FU_OvCa_Event"), all_vars(!is.na(.)))%>%
  mutate_at(vars("race","educ","income", "menopause", "smoking", "alcohol", "BCduration", "HRT1"), factor)%>% 
  mutate(BMI_3c = case_when(BMI<25 ~ 0, 
                            BMI>=25 & BMI<30 ~ 1, 
                            BMI>=30 ~ 2))

# Uterine Cancer ---
dat3 <- dataUE %>%
  filter_at(vars(all_of(covs2),  "AgeExact_Baseline", "FU_UECa_EOFAgeExact_hyst", "FU_UECa_Event"), all_vars(!is.na(.)))%>%
  mutate_at(vars("race","educ","income", "menopause", "smoking", "alcohol", "BCduration", "HRT2"), factor)%>% 
  mutate(BMI_3c = case_when(BMI<25 ~ 0, 
                            BMI>=25 & BMI<30 ~ 1, 
                            BMI>=30 ~ 2))

# Race ----
## Black ---- 

# Beauty 
ex <- c('blush', 'eyeliner', 'eyeshadow', 'foundation', 'lipstick', 'mascara', 'makeupremover', 
        'perfume', 'artificialnail', 'cuticlecream', 'nailpolish', 'nailremover')

# Use the function in function.R' please run the function codes in function.R first. 
# from here; make the # right 

dat1 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% group_by(race) %>% summarise(n=n(), n_event=sum(FU_BCInvD_Event), py=sum(FU_BCInvD_EOFAgeExact - AgeExact_Baseline))
dat2 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% group_by(race) %>% summarise(n=n(), n_event=sum(FU_OvCa_Event), py=sum(FU_OvCa_EOFAgeExact_ooph - AgeExact_Baseline))
dat3 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% group_by(race) %>% summarise(n=n(), n_event=sum(FU_UECa_Event), py=sum(FU_UECa_EOFAgeExact_hyst - AgeExact_Baseline))

BC1 <- dat1 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% filter(race%in%c(0,1)) %>% mutate(race=relevel(race, ref="1")) %>% BC_race_fun()
Ov1 <- dat2 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% filter(race%in%c(0,1)) %>% mutate(race=relevel(race, ref="1")) %>% Ov_race_fun()
UE1 <- dat3 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% filter(race%in%c(0,1)) %>% mutate(race=relevel(race, ref="1")) %>% UE_race_fun()

# Hygiene 
ex <- c('bathgel', 'deodorant', 'douche', 'mouthwash', 'shavingcream', 'talcarm', 'talcvaginal', 'talcother')

dat1 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% group_by(race) %>% summarise(n=n(), n_event=sum(FU_BCInvD_Event), py=sum(FU_BCInvD_EOFAgeExact - AgeExact_Baseline))
dat2 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% group_by(race) %>% summarise(n=n(), n_event=sum(FU_OvCa_Event), py=sum(FU_OvCa_EOFAgeExact_ooph - AgeExact_Baseline))
dat3 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% group_by(race) %>% summarise(n=n(), n_event=sum(FU_UECa_Event), py=sum(FU_UECa_EOFAgeExact_hyst - AgeExact_Baseline))

# Use the function in function.R' please run the function codes in function.R first. 
BC2 <- dat1 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% filter(race%in%c(0,1)) %>% mutate(race=relevel(race, ref="1")) %>% BC_race_fun()
Ov2 <- dat2 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% filter(race%in%c(0,1)) %>% mutate(race=relevel(race, ref="1")) %>% Ov_race_fun()
UE2 <- dat3 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% filter(race%in%c(0,1)) %>% mutate(race=relevel(race, ref="1")) %>% UE_race_fun()

# Skincare 
ex <- c('antiaging', 'agespotlightener', 'babyoil', 'blemish', 'bodylotion', 'cleansing', 'facecream', 'mask', 
        'footcream', 'handlotion', 'moisturizer','petroleumjelly', 'skinlightener', 'tanner')

dat1 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% group_by(race) %>% summarise(n=n(), n_event=sum(FU_BCInvD_Event), py=sum(FU_BCInvD_EOFAgeExact - AgeExact_Baseline))
dat2 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% group_by(race) %>% summarise(n=n(), n_event=sum(FU_OvCa_Event), py=sum(FU_OvCa_EOFAgeExact_ooph - AgeExact_Baseline))
dat3 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% group_by(race) %>% summarise(n=n(), n_event=sum(FU_UECa_Event), py=sum(FU_UECa_EOFAgeExact_hyst - AgeExact_Baseline))


# Use the function in function.R' please run the function codes in function.R first. 
BC3 <- dat1 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% filter(race%in%c(0,1)) %>% mutate(race=relevel(race, ref="1")) %>% BC_race_fun()
Ov3 <- dat2 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% filter(race%in%c(0,1)) %>% mutate(race=relevel(race, ref="1")) %>% Ov_race_fun()
ex <- c('antiaging', 'babyoil', 'blemish', 'bodylotion', 'cleansing', 'facecream', 'mask', 
        'footcream', 'handlotion', 'moisturizer','petroleumjelly')
UE3 <- dat3 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% filter(race%in%c(0,1)) %>% mutate(race=relevel(race, ref="1")) %>% UE_race_fun()


## White ---- 
# Beauty 
ex <- c('blush', 'eyeliner', 'eyeshadow', 'foundation', 'lipstick', 'mascara', 'makeupremover', 
        'perfume', 'artificialnail', 'cuticlecream', 'nailpolish', 'nailremover')

# Use the function in function.R' please run the function codes in function.R first. 
# from here; make the # right 
BC1 <- dat1 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% filter(race%in%c(0,1)) %>% mutate(race=relevel(race, ref="0")) %>% BC_race_fun()
Ov1 <- dat2 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% filter(race%in%c(0,1)) %>% mutate(race=relevel(race, ref="0")) %>% Ov_race_fun()
UE1 <- dat3 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% filter(race%in%c(0,1)) %>% mutate(race=relevel(race, ref="0")) %>% UE_race_fun()

# Hygiene 
ex <- c('bathgel', 'deodorant', 'douche', 'mouthwash', 'shavingcream', 'talcarm', 'talcvaginal', 'talcother')

# Use the function in function.R' please run the function codes in function.R first. 
BC2 <- dat1 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% filter(race%in%c(0,1)) %>% mutate(race=relevel(race, ref="0")) %>% BC_race_fun()
Ov2 <- dat2 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% filter(race%in%c(0,1)) %>% mutate(race=relevel(race, ref="0")) %>% Ov_race_fun()
UE2 <- dat3 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% filter(race%in%c(0,1)) %>% mutate(race=relevel(race, ref="0")) %>% UE_race_fun()

# Skincare 
ex <- c('antiaging', 'agespotlightener', 'babyoil', 'blemish', 'bodylotion', 'cleansing', 'facecream', 'mask', 
        'footcream', 'handlotion', 'moisturizer','petroleumjelly', 'skinlightener', 'tanner')

# Use the function in function.R' please run the function codes in function.R first. 
BC3 <- dat1 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% filter(race%in%c(0,1)) %>% mutate(race=relevel(race, ref="0")) %>% BC_race_fun()
Ov3 <- dat2 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% filter(race%in%c(0,1)) %>% mutate(race=relevel(race, ref="0")) %>% Ov_race_fun()
UE3 <- dat3 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% filter(race%in%c(0,1)) %>% mutate(race=relevel(race, ref="0")) %>% UE_race_fun()


## P-for-interaction for race ----
# Beauty 
ex <- c('blush', 'eyeliner', 'eyeshadow', 'foundation', 'lipstick', 'mascara', 'makeupremover', 
        'perfume', 'artificialnail', 'cuticlecream', 'nailpolish', 'nailremover')
BC1_inter <- dat1 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% filter(race%in%c(0,1))%>% BC_race_pinter_fun()
Ov1_inter <- dat2 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% filter(race%in%c(0,1))%>% Ov_race_pinter_fun()
UE1_inter <- dat3 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% filter(race%in%c(0,1))%>% UE_race_pinter_fun()

# Hygiene 
ex <- c('bathgel', 'deodorant', 'douche', 'mouthwash', 'shavingcream', 'talcarm', 'talcvaginal', 'talcother')
BC2_inter <- dat1 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% filter(race%in%c(0,1))%>% BC_race_pinter_fun()
Ov2_inter <- dat2 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% filter(race%in%c(0,1))%>% Ov_race_pinter_fun()
UE2_inter <- dat3 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% filter(race%in%c(0,1))%>% UE_race_pinter_fun()

# Skincare 
ex <- c('antiaging', 'agespotlightener', 'babyoil', 'blemish', 'bodylotion', 'cleansing', 'facecream', 'mask', 
        'footcream', 'handlotion', 'moisturizer','petroleumjelly', 'skinlightener', 'tanner')
BC3_inter <- dat1 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% filter(race%in%c(0,1))%>% BC_race_pinter_fun()
Ov3_inter <- dat2 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% filter(race%in%c(0,1))%>% Ov_race_pinter_fun()
UE3_inter <- dat3 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% filter(race%in%c(0,1))%>% UE_race_pinter_fun()

# BMI ----
## <25 ---- 


# Beauty 
ex <- c('blush', 'eyeliner', 'eyeshadow', 'foundation', 'lipstick', 'mascara', 'makeupremover', 
        'perfume', 'artificialnail', 'cuticlecream', 'nailpolish', 'nailremover')

# Use the function in function.R' please run the function codes in function.R first. 
# from here; make the # right 

dat1 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% group_by(BMI_3c) %>% summarise(n=n(), n_event=sum(FU_BCInvD_Event), py=sum(FU_BCInvD_EOFAgeExact - AgeExact_Baseline))
dat2 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% group_by(BMI_3c) %>% summarise(n=n(), n_event=sum(FU_OvCa_Event), py=sum(FU_OvCa_EOFAgeExact_ooph - AgeExact_Baseline))
dat3 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% group_by(BMI_3c) %>% summarise(n=n(), n_event=sum(FU_UECa_Event), py=sum(FU_UECa_EOFAgeExact_hyst - AgeExact_Baseline))


BC1 <- dat1 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% mutate(BMI_3c=relevel(as.factor(BMI_3c), ref="0")) %>% BC_3BMI_fun()
Ov1 <- dat2 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% mutate(BMI_3c=relevel(as.factor(BMI_3c), ref="0")) %>% Ov_3BMI_fun()
UE1 <- dat3 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% mutate(BMI_3c=relevel(as.factor(BMI_3c), ref="0")) %>% UE_3BMI_fun()

# Hygiene 
ex <- c('bathgel', 'deodorant', 'douche', 'mouthwash', 'shavingcream', 'talcarm', 'talcvaginal', 'talcother')

dat1 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% group_by(BMI_3c) %>% summarise(n=n(), n_event=sum(FU_BCInvD_Event), py=sum(FU_BCInvD_EOFAgeExact - AgeExact_Baseline))
dat2 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% group_by(BMI_3c) %>% summarise(n=n(), n_event=sum(FU_OvCa_Event), py=sum(FU_OvCa_EOFAgeExact_ooph - AgeExact_Baseline))
dat3 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% group_by(BMI_3c) %>% summarise(n=n(), n_event=sum(FU_UECa_Event), py=sum(FU_UECa_EOFAgeExact_hyst - AgeExact_Baseline))


BC2 <- dat1 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% mutate(BMI_3c=relevel(as.factor(BMI_3c), ref="0")) %>% BC_3BMI_fun()
Ov2 <- dat2 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% mutate(BMI_3c=relevel(as.factor(BMI_3c), ref="0")) %>% Ov_3BMI_fun()
UE2 <- dat3 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% mutate(BMI_3c=relevel(as.factor(BMI_3c), ref="0")) %>% UE_3BMI_fun()

# Skincare 
ex <- c('antiaging', 'agespotlightener', 'babyoil', 'blemish', 'bodylotion', 'cleansing', 'facecream', 'mask', 
        'footcream', 'handlotion', 'moisturizer','petroleumjelly', 'skinlightener', 'tanner')

dat1 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% group_by(BMI_3c) %>% summarise(n=n(), n_event=sum(FU_BCInvD_Event), py=sum(FU_BCInvD_EOFAgeExact - AgeExact_Baseline))
dat2 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% group_by(BMI_3c) %>% summarise(n=n(), n_event=sum(FU_OvCa_Event), py=sum(FU_OvCa_EOFAgeExact_ooph - AgeExact_Baseline))
dat3 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% group_by(BMI_3c) %>% summarise(n=n(), n_event=sum(FU_UECa_Event), py=sum(FU_UECa_EOFAgeExact_hyst - AgeExact_Baseline))

BC3 <- dat1 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% mutate(BMI_3c=relevel(as.factor(BMI_3c), ref="0")) %>% BC_3BMI_fun()
Ov3 <- dat2 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% mutate(BMI_3c=relevel(as.factor(BMI_3c), ref="0")) %>% Ov_3BMI_fun()
UE3 <- dat3 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% mutate(BMI_3c=relevel(as.factor(BMI_3c), ref="0")) %>% UE_3BMI_fun()


## >=25 and <30 ---- 

# Beauty 
ex <- c('blush', 'eyeliner', 'eyeshadow', 'foundation', 'lipstick', 'mascara', 'makeupremover', 
        'perfume', 'artificialnail', 'cuticlecream', 'nailpolish', 'nailremover')

# Use the function in function.R' please run the function codes in function.R first. 
# from here; make the # right 

BC1 <- dat1 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% mutate(BMI_3c=relevel(as.factor(BMI_3c), ref="1")) %>% BC_3BMI_fun()
Ov1 <- dat2 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% mutate(BMI_3c=relevel(as.factor(BMI_3c), ref="1")) %>% Ov_3BMI_fun()
UE1 <- dat3 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% mutate(BMI_3c=relevel(as.factor(BMI_3c), ref="1")) %>% UE_3BMI_fun()

# Hygiene 
ex <- c('bathgel', 'deodorant', 'douche', 'mouthwash', 'shavingcream', 'talcarm', 'talcvaginal', 'talcother')

BC2 <- dat1 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% mutate(BMI_3c=relevel(as.factor(BMI_3c), ref="1")) %>% BC_3BMI_fun()
Ov2 <- dat2 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% mutate(BMI_3c=relevel(as.factor(BMI_3c), ref="1")) %>% Ov_3BMI_fun()
UE2 <- dat3 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% mutate(BMI_3c=relevel(as.factor(BMI_3c), ref="1")) %>% UE_3BMI_fun()

# Skincare 
ex <- c('antiaging', 'agespotlightener', 'babyoil', 'blemish', 'bodylotion', 'cleansing', 'facecream', 'mask', 
        'footcream', 'handlotion', 'moisturizer','petroleumjelly', 'skinlightener', 'tanner')

BC3 <- dat1 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% mutate(BMI_3c=relevel(as.factor(BMI_3c), ref="1")) %>% BC_3BMI_fun()
Ov3 <- dat2 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% mutate(BMI_3c=relevel(as.factor(BMI_3c), ref="1")) %>% Ov_3BMI_fun()
UE3 <- dat3 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% mutate(BMI_3c=relevel(as.factor(BMI_3c), ref="1")) %>% UE_3BMI_fun()

## >=30 ---- 

# Beauty 
ex <- c('blush', 'eyeliner', 'eyeshadow', 'foundation', 'lipstick', 'mascara', 'makeupremover', 
        'perfume', 'artificialnail', 'cuticlecream', 'nailpolish', 'nailremover')

# Use the function in function.R' please run the function codes in function.R first. 
# from here; make the # right 

BC1 <- dat1 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% mutate(BMI_3c=relevel(as.factor(BMI_3c), ref="2")) %>% BC_3BMI_fun()
Ov1 <- dat2 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% mutate(BMI_3c=relevel(as.factor(BMI_3c), ref="2")) %>% Ov_3BMI_fun()
UE1 <- dat3 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% mutate(BMI_3c=relevel(as.factor(BMI_3c), ref="2")) %>% UE_3BMI_fun()

# Hygiene 
ex <- c('bathgel', 'deodorant', 'douche', 'mouthwash', 'shavingcream', 'talcarm', 'talcvaginal', 'talcother')

BC2 <- dat1 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% mutate(BMI_3c=relevel(as.factor(BMI_3c), ref="2")) %>% BC_3BMI_fun()
Ov2 <- dat2 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% mutate(BMI_3c=relevel(as.factor(BMI_3c), ref="2")) %>% Ov_3BMI_fun()
UE2 <- dat3 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% mutate(BMI_3c=relevel(as.factor(BMI_3c), ref="2")) %>% UE_3BMI_fun()

# Skincare 
ex <- c('antiaging', 'agespotlightener', 'babyoil', 'blemish', 'bodylotion', 'cleansing', 'facecream', 'mask', 
        'footcream', 'handlotion', 'moisturizer','petroleumjelly', 'skinlightener', 'tanner')

dat1 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% group_by(BMI_3c) %>% summarise(n=n(), n_event=sum(FU_BCInvD_Event))
dat2 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% group_by(BMI_3c) %>% summarise(n=n(), n_event=sum(FU_OvCa_Event))
dat3 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% group_by(BMI_3c) %>% summarise(n=n(), n_event=sum(FU_UECa_Event))

BC3 <- dat1 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% mutate(BMI_3c=relevel(as.factor(BMI_3c), ref="2")) %>% BC_3BMI_fun()
Ov3 <- dat2 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% mutate(BMI_3c=relevel(as.factor(BMI_3c), ref="2")) %>% Ov_3BMI_fun()
UE3 <- dat3 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% mutate(BMI_3c=relevel(as.factor(BMI_3c), ref="2")) %>% UE_3BMI_fun()


## P-for-interaction for BMI ----
# Beauty 
ex <- c('blush', 'eyeliner', 'eyeshadow', 'foundation', 'lipstick', 'mascara', 'makeupremover', 
        'perfume', 'artificialnail', 'cuticlecream', 'nailpolish', 'nailremover')
BC1_inter <- dat1 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% BC_3BMI_pinter_fun()
Ov1_inter <- dat2 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% Ov_3BMI_pinter_fun()
UE1_inter <- dat3 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% UE_3BMI_pinter_fun()

# Hygiene 
ex <- c('bathgel', 'deodorant', 'douche', 'mouthwash', 'shavingcream', 'talcarm', 'talcvaginal', 'talcother')
BC2_inter <- dat1 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% BC_3BMI_pinter_fun()
Ov2_inter <- dat2 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% Ov_3BMI_pinter_fun()
UE2_inter <- dat3 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% UE_3BMI_pinter_fun()

# Skincare 
ex <- c('antiaging', 'agespotlightener', 'babyoil', 'blemish', 'bodylotion', 'cleansing', 'facecream', 'mask', 
        'footcream', 'handlotion', 'moisturizer','petroleumjelly', 'skinlightener', 'tanner')
BC3_inter <- dat1 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% BC_3BMI_pinter_fun()
Ov3_inter <- dat2 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% Ov_3BMI_pinter_fun()
UE3_inter <- dat3 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% UE_3BMI_pinter_fun()

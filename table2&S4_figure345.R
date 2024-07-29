# Table 2 & Table S4 & Figure 3-5

library(tidyverse)
library(survival) # coxph
library(survminer) # coxph
library(stringr)
library(reshape2)
library(qgcomp)
library(naniar)
library(rms)
library(qgcompint)
library(lmtest)

# Load data ---

dataBC <- read_csv("clean_data/dataBC.csv") 
dataOv <- read_csv("clean_data/dataOv.csv") 
dataUE <- read_csv("clean_data/dataUE.csv") 

BCpre <- read_csv("clean_data/BCpre.csv"); BCpost <- read_csv("clean_data/BCpost.csv"); BCmeno <- read_csv("clean_data/BCmeno.csv")
Ovpre <- read_csv("clean_data/Ovpre.csv"); Ovpost <- read_csv("clean_data/Ovpost.csv"); Ovmeno <- read_csv("clean_data/Ovmeno.csv")
UEpre <- read_csv("clean_data/UEpre.csv"); UEpost <- read_csv("clean_data/UEpost.csv"); UEmeno <- read_csv("clean_data/UEmeno.csv")

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
  mutate_at(vars("race","educ","income", "menopause", "smoking", "alcohol", "BCduration", "HRT1"), factor) 
predat1 <- BCpre %>%
  filter_at(vars(all_of(covs1),  "tstart", "tstop", "event3"), all_vars(!is.na(.)))%>%
  mutate_at(vars("race","educ","income", "menopause", "smoking", "alcohol", "BCduration", "HRT1"), factor)
postdat1 <- BCpost %>%
  filter_at(vars(all_of(covs1),  "tstart", "tstop", "event3"), all_vars(!is.na(.)))%>%
  mutate_at(vars("race","educ","income", "menopause", "smoking", "alcohol", "BCduration", "HRT1"), factor)
menodat1 <- BCmeno %>%
  filter_at(vars(all_of(covs1),  "tstart", "tstop", "event3"), all_vars(!is.na(.)))%>%
  mutate_at(vars("race","educ","income", "menopause", "smoking", "alcohol", "BCduration", "HRT1"), factor)

# Ovarian Cancer ---
dat2 <- dataOv %>%
  filter_at(vars(all_of(covs1), "AgeExact_Baseline", "FU_OvCa_EOFAgeExact_ooph", "FU_OvCa_Event"), all_vars(!is.na(.)))%>%
  mutate_at(vars("race","educ","income", "menopause", "smoking", "alcohol", "BCduration", "HRT1"), factor)
predat2 <- Ovpre %>%
  filter_at(vars(all_of(covs1),  "tstart", "tstop", "event3"), all_vars(!is.na(.)))%>%
  mutate_at(vars("race","educ","income", "menopause", "smoking", "alcohol", "BCduration", "HRT1"), factor)
postdat2 <- Ovpost %>%
  filter_at(vars(all_of(covs1),  "tstart", "tstop", "event3"), all_vars(!is.na(.)))%>%
  mutate_at(vars("race","educ","income", "menopause", "smoking", "alcohol", "BCduration", "HRT1"), factor)
menodat2 <- Ovmeno %>%
  filter_at(vars(all_of(covs1),  "tstart", "tstop", "event3"), all_vars(!is.na(.)))%>%
  mutate_at(vars("race","educ","income", "menopause", "smoking", "alcohol", "BCduration", "HRT1"), factor)

# Uterine Cancer ---
dat3 <- dataUE %>%
  filter_at(vars(all_of(covs2),  "AgeExact_Baseline", "FU_UECa_EOFAgeExact_hyst", "FU_UECa_Event"), all_vars(!is.na(.)))%>%
  mutate_at(vars("race","educ","income", "menopause", "smoking", "alcohol", "BCduration", "HRT2"), factor)
predat3 <- UEpre %>%
  filter_at(vars(all_of(covs2), "tstart", "tstop", "event3"), all_vars(!is.na(.)))%>%
  mutate_at(vars("race","educ","income", "menopause", "smoking", "alcohol", "BCduration", "HRT2"), factor)
postdat3 <- UEpost %>%
  filter_at(vars(all_of(covs2),  "tstart", "tstop", "event3"), all_vars(!is.na(.)))%>%
  mutate_at(vars("race","educ","income", "menopause", "smoking", "alcohol", "BCduration", "HRT2"), factor)
menodat3 <- UEmeno %>%
  filter_at(vars(all_of(covs2),  "tstart", "tstop", "event3"), all_vars(!is.na(.)))%>%
  mutate_at(vars("race","educ","income", "menopause", "smoking", "alcohol", "BCduration", "HRT2"), factor)

# Pre and post-menopausal cancers ----
# Beauty 
ex <- c('blush', 'eyeliner', 'eyeshadow', 'foundation', 'lipstick', 'mascara', 'makeupremover', 
        'perfume', 'artificialnail', 'cuticlecream', 'nailpolish', 'nailremover')

# Use the function in function.R' please run the function codes in function.R first. 
BC1 <- dat1 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% BC_fun()
Ov1 <- dat2 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% Ov_fun()
UE1 <- dat3 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% UE_fun()

test <- menodat2 %>% filter_at(all_of(ex), all_vars(!is.na(.)))%>% mutate(prepost = factor(prepost, levels=c(1,0)))
tt <- qgcomp.cox.noboot(Surv(tstart, tstop, event3) ~ race + educ + income + BMI + smoking +
                    PA + BCduration + alcohol + HRT1 + race:prepost + educ:prepost +
                    income:prepost + BMI:prepost + smoking:prepost + PA:prepost +
                    BCduration:prepost + alcohol:prepost + HRT1:prepost + blush +
                    eyeliner + eyeshadow + foundation + lipstick + mascara +
                    makeupremover + perfume + artificialnail + cuticlecream +
                    nailpolish + nailremover + blush:prepost + eyeliner:prepost +
                    eyeshadow:prepost + foundation:prepost + lipstick:prepost +
                    mascara:prepost + makeupremover:prepost + perfume:prepost +
                    artificialnail:prepost + cuticlecream:prepost + nailpolish:prepost +
                    nailremover:prepost + prepost + strata(prepost),expnms = ex, q=NULL, data=test)

lhs = "Surv(AgeExact_Baseline, FU_OvCa_EOFAgeExact_ooph, FU_OvCa_Event)"
ex_string = paste0(ex, collapse=" + ")
rhs1 = paste0(ex_string)
crude_form = as.formula(paste(lhs,"~",rhs1))
rhs2 = paste0("race+educ+income+menopause*rcs(BMI, quantile(BMI, c(0, .5, .35, .65, .95, 1), na.rm = TRUE))+smoking+alcohol+BCduration+HRT1+PA+", ex_string)
adjusted_form = as.formula(paste(lhs,"~",rhs2))

test <- dat2%>%filter_at(all_of(ex), all_vars(!is.na(.)))
tt2 <- qgcomp.cox.noboot(adjusted_form, expnms = ex, q=NULL, data=test)
exp(summary(tt2)$coefficients[1])


lhs = "Surv(tstart, tstop, event3)"
ex_string = paste0(ex, collapse=" + ")
rhs1 = paste0(ex_string)
crude_form = as.formula(paste(lhs,"~",rhs1))
rhs2 = paste0("race+educ+income+rcs(BMI, quantile(BMI, c(0, .5, .35, .65, .95, 1), na.rm = TRUE))+smoking+alcohol+BCduration+HRT1+PA+", ex_string)
adjusted_form = as.formula(paste(lhs,"~",rhs2))

test <- predat2%>%filter_at(all_of(ex), all_vars(!is.na(.)))
tt2 <- qgcomp.cox.noboot(adjusted_form, expnms = ex, q=NULL, data=test)
exp(summary(tt2)$coefficients[1])


test <- postdat2%>%filter_at(all_of(ex), all_vars(!is.na(.)))
tt2 <- qgcomp.cox.noboot(adjusted_form, expnms = ex, q=NULL, data=test)
exp(summary(tt2)$coefficients)

# Hygiene 
ex <- c('bathgel', 'deodorant', 'douche', 'mouthwash', 'shavingcream', 'talcarm', 'talcvaginal', 'talcother')

# Use the function in function.R' please run the function codes in function.R first. 
BC2 <- dat1 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% BC_fun()
Ov2 <- dat2 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% Ov_fun()
UE2 <- dat3 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% UE_fun()

# Skincare 
ex <- c('antiaging', 'agespotlightener', 'babyoil', 'blemish', 'bodylotion', 'cleansing', 'facecream', 'mask', 
        'footcream', 'handlotion', 'moisturizer','petroleumjelly', 'skinlightener', 'tanner')

# Use the function in function.R' please run the function codes in function.R first. 
BC3 <- dat1 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% BC_fun()
Ov3 <- dat2 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% Ov_fun()
UE3 <- dat3 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% UE_fun()

# Premenopausal cancers ----
# Beauty 
ex <- c('blush', 'eyeliner', 'eyeshadow', 'foundation', 'lipstick', 'mascara', 'makeupremover', 
           'perfume', 'artificialnail', 'cuticlecream', 'nailpolish', 'nailremover')

# Use the function in function.R' please run the function codes in function.R first. 
preBC1 <- predat1 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% meno_fun()
preOv1 <- predat2 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% meno_fun()
preUE1 <- predat3 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% UE_meno_fun()

# Hygiene 
ex <- c('bathgel', 'deodorant', 'douche', 'mouthwash', 'shavingcream', 'talcarm', 'talcvaginal', 'talcother')

# Use the function in function.R' please run the function codes in function.R first. 
preBC2 <- predat1 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% meno_fun()
preOv2 <- predat2 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% meno_fun()
preUE2 <- predat3 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% UE_meno_fun()

# Skincare 
ex <- c('antiaging', 'agespotlightener', 'babyoil', 'blemish', 'bodylotion', 'cleansing', 'facecream', 'mask', 
        'footcream', 'handlotion', 'moisturizer','petroleumjelly', 'skinlightener', 'tanner')

# Use the function in function.R' please run the function codes in function.R first. 
preBC3 <- predat1 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% meno_fun()
ex <- c('antiaging', 'babyoil', 'blemish', 'bodylotion', 'cleansing', 'facecream', 'mask', 
        'footcream', 'handlotion', 'moisturizer','petroleumjelly', 'tanner') # remove two products due to low case counts 
preOv3 <- predat2 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% meno_fun()
ex <- c('antiaging', 'agespotlightener', 'babyoil', 'blemish', 'bodylotion', 'cleansing', 'facecream', 'mask', 
        'footcream', 'handlotion', 'moisturizer','petroleumjelly', 'skinlightener', 'tanner')
preUE3 <- predat3 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% UE_meno_fun()

# Postmenopausal cancers ----
# Beauty 
ex <- c('blush', 'eyeliner', 'eyeshadow', 'foundation', 'lipstick', 'mascara', 'makeupremover', 
        'perfume', 'artificialnail', 'cuticlecream', 'nailpolish', 'nailremover')

# Use the function in function.R' please run the function codes in function.R first. 
postBC1 <- postdat1 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% meno_fun()
postOv1 <- postdat2 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% meno_fun()
postUE1 <- postdat3 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% UE_meno_fun()

# Hygiene 
ex <- c('bathgel', 'deodorant', 'douche', 'mouthwash', 'shavingcream', 'talcarm', 'talcvaginal', 'talcother')

# Use the function in function.R' please run the function codes in function.R first. 
postBC2 <- postdat1 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% meno_fun()
postOv2 <- postdat2 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% meno_fun()
postUE2 <- postdat3 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% UE_meno_fun()

# Skincare 
ex <- c('antiaging', 'agespotlightener', 'babyoil', 'blemish', 'bodylotion', 'cleansing', 'facecream', 'mask', 
        'footcream', 'handlotion', 'moisturizer','petroleumjelly', 'skinlightener', 'tanner')

# Use the function in function.R' please run the function codes in function.R first. 
postBC3 <- postdat1 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% meno_fun()
postOv3 <- postdat2 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% meno_fun()
postUE3 <- postdat3 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% UE_meno_fun()

# p-values for heterogeneity ----
# Beauty 
ex <- c('blush', 'eyeliner', 'eyeshadow', 'foundation', 'lipstick', 'mascara', 'makeupremover', 
        'perfume', 'artificialnail', 'cuticlecream', 'nailpolish', 'nailremover')

# Use the function in function.R' please run the function codes in function.R first. 
# with and without "filter(tstart < tstop)" did not make a difference. There are very few people who didn't contribute any person time in the analysis and was  
interBC1 <- menodat1 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% filter(tstart < tstop) %>% meno_pinter_fun()
interOv1 <- menodat2 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% filter(tstart < tstop) %>% meno_pinter_fun()
interUE1 <- menodat3 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% filter(tstart < tstop) %>% UE_meno_pinter_fun()

# Hygiene 
ex <- c('bathgel', 'deodorant', 'douche', 'mouthwash', 'shavingcream', 'talcarm', 'talcvaginal', 'talcother')

# Use the function in function.R' please run the function codes in function.R first. 
interBC2 <- menodat1 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% filter(tstart < tstop) %>% meno_pinter_fun()
interOv2 <- menodat2 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% filter(tstart < tstop) %>% meno_pinter_fun()
interUE2 <- menodat3 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% filter(tstart < tstop) %>% UE_meno_pinter_fun()

# Skincare 
ex <- c('antiaging', 'agespotlightener', 'babyoil', 'blemish', 'bodylotion', 'cleansing', 'facecream', 'mask', 
        'footcream', 'handlotion', 'moisturizer','petroleumjelly', 'skinlightener', 'tanner')

# Use the function in function.R' please run the function codes in function.R first. 
interBC3 <- menodat1 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% meno_pinter_fun()
# interOv3 <- menodat2 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% meno_pinter_fun() # didnt run because of different products included in pre and post menopausal cancer
interUE3 <- menodat3 %>% filter_at(all_of(ex), all_vars(!is.na(.))) %>% UE_meno_pinter_fun()

# Troubleshoot the issue of p-for-het 
# Weird p-for-het for ovarian cancer; since there is a warning about convergence 
# "2: In agreg.fit(X, Y, istrat, offset, init, control, weights = weights, Loglik converged before variable  2,32 ; beta may be infinite.
# Try removing variables that have large beta; such as BMI splines; and try crude analysis; 
# no convergence warning, but the p-for-het still seems off. 

lhs = "Surv(tstart, tstop, event3)"
ex_string = paste0(ex, collapse=" + ")
ex_string2 = paste0(ex, collapse=":prepost+ ")

rhs1 = paste0("race+educ+income+BMI+smoking+PA+BCduration+alcohol+HRT1+
                 race:prepost+educ:prepost+income:prepost+BMI:prepost+
                smoking:prepost+PA:prepost+BCduration:prepost+alcohol:prepost+HRT1:prepost+", ex_string)
rhs2 = paste0("race+educ+income+BMI+smoking+PA+BCduration+alcohol+HRT1+
                race:prepost+educ:prepost+income:prepost+BMI:prepost+
                smoking:prepost+PA:prepost+BCduration:prepost+alcohol:prepost+HRT1:prepost+",ex_string,"+", ex_string2, ":prepost")

rhs1 = paste0(ex_string)
rhs2 = paste0(ex_string,"+", ex_string2, ":prepost")

reduced = as.formula(paste(lhs,"~",rhs1))
full = as.formula(paste(lhs,"~",rhs2))

mod1 <- qgcomp.cox.noboot(reduced, expnms = ex, q=NULL, data = menodat2)
mod2 <- qgcomp.cox.noboot(full, expnms = ex, q=NULL, data = menodat2)

mod3 <- qgcomp.cox.noboot(full, expnms = ex, q=NULL, data = menodat2[menodat2$prepost==0,])
mod4 <- qgcomp.cox.noboot(full, expnms = ex, q=NULL, data = menodat2[menodat2$prepost==1,])

exp(mod3$psi); exp(mod3$ci)
exp(mod4$psi); exp(mod4$ci)

round(waldtest(mod1$fit, mod2$fit)$`Pr(>Chisq)`[2],4)



ex <- c('blush', 'eyeliner', 'eyeshadow', 'foundation', 'lipstick', 'mascara', 'makeupremover', 
        'perfume', 'artificialnail', 'cuticlecream', 'nailpolish', 'nailremover')
lhs = "Surv(tstart, tstop, event3)"
ex_string = paste0(ex, collapse=" + ")
ex_string2 = paste0(ex, collapse=":prepost+ ")

rhs1 = paste0("race+educ+income+BMI+smoking+PA+BCduration+alcohol+HRT1+
                 race:prepost+educ:prepost+income:prepost+BMI:prepost+
                smoking:prepost+PA:prepost+BCduration:prepost+alcohol:prepost+HRT1:prepost+", ex_string)
rhs2 = paste0("race+educ+income+BMI+smoking+PA+BCduration+alcohol+HRT1+
                race:prepost+educ:prepost+income:prepost+BMI:prepost+
                smoking:prepost+PA:prepost+BCduration:prepost+alcohol:prepost+HRT1:prepost+",ex_string,"+", ex_string2, ":prepost")

reduced = as.formula(paste(lhs,"~",rhs1))
full = as.formula(paste(lhs,"~",rhs2))

# BC
prepost1 <- coxph(reduced, ties='breslow', data=menodat1) 
prepost2 <- coxph(full , ties='breslow',  data=menodat1) 
round(waldtest(prepost1, prepost2)$`Pr(>Chisq)`[2],4)

# Ov
prepost1 <- coxph(reduced, ties='breslow', data=menodat2) 
prepost2 <- coxph(full , ties='breslow',  data=menodat2) 
round(waldtest(prepost1, prepost2)$`Pr(>Chisq)`[2],4)

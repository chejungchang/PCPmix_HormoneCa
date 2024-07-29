# Table S3 & Figure 3-5

library(tidyverse)
library(survival) # coxph
library(survminer) # coxph
library(stringr)
library(reshape2)
library(qgcomp)
library(naniar)
library(rms)
library(lmtest)
library(naniar)


dataBC <- read_csv("clean_data/dataBC.csv") 
dataOv <- read_csv("clean_data/dataOv.csv") 
dataUE <- read_csv("clean_data/dataUE.csv") 

covs1 <- c("race","educ", "income","menopause", "smoking", "alcohol","BCduration", "HRT1", "PA", "BMI")
covs2 <- c("race","educ", "income","menopause", "smoking", "alcohol","BCduration", "HRT2", "PA", "BMI")
productname = c('blush', 'eyeliner', 'eyeshadow', 'foundation', 'lipstick', 'mascara', 'makeupremover', 
                'perfume', 'artificialnail', 'cuticlecream', 'nailpolish', 'nailremover', 
                'conditioner', 'hairfood', 'hairspray', 'hairgel', 'minoxidil', 'pomade', 'shampoo', 
                'bathgel', 'deodorant', 'douche', 'mouthwash', 'shavingcream', 'talcarm', 'talcvaginal', 'talcother',    
                'antiaging', 'agespotlightener', 'babyoil', 'blemish', 'bodylotion', 'cleansing', 'facecream', 'mask', 
                'footcream', 'handlotion', 'moisturizer','petroleumjelly', 'skinlightener',  'tanner')  

# Breast cancer ----

data1 <- dataBC %>%
  as_tibble()%>%
  select_at(vars(all_of(productname), all_of(covs1), "FU_BCInvD_EOFAgeExact","AgeExact_Baseline","FU_BCInvD_Event"))%>%
  mutate_at(vars(-BMI, -PA, -AgeExact_Baseline, -FU_BCInvD_EOFAgeExact, -FU_BCInvD_Event, -all_of(productname)), factor)%>%
  filter_at(vars(all_of(covs1)), all_vars(!is.na(.))) 


# create list column 
by_product <- data1 %>%
  pivot_longer(all_of(productname), names_to="product", values_to = "frequency")%>%
  group_by(product) %>% nest()

# Crude model 
crude_mod <-  function(df) {
  df %>%
    coxph(Surv(AgeExact_Baseline, FU_BCInvD_EOFAgeExact, FU_BCInvD_Event) ~ frequency, ties='breslow', data=.) %>% 
    broom::tidy(.,exponentiate = TRUE, conf.int= TRUE) %>%
    select(term, estimate, std.error, conf.low, conf.high, p.value)
}

# Main model  
mod1 <-  function(df) {
  df %>%
    coxph(Surv(AgeExact_Baseline, FU_BCInvD_EOFAgeExact, FU_BCInvD_Event) ~ frequency + race + educ + income + menopause*rcs(BMI, quantile(BMI, c(0, .5, .35, .65, .95, 1), na.rm = TRUE)) + 
            smoking + PA + BCduration + HRT1 + alcohol , ties='breslow', data=.) %>% 
    broom::tidy(.,exponentiate = TRUE, conf.int= TRUE) %>%
    slice_head(n = 1) %>%
    select(term, estimate, std.error, conf.low, conf.high, p.value)
}




samplesize <- function(df){
  df %>% filter_at(vars(frequency, race, educ, income, menopause, BMI, smoking, PA, BCduration, HRT1, alcohol), all_vars(!is.na(.))) %>% summarise(n=nrow(.), n_event=sum(FU_BCInvD_Event))
}




c_list <- by_product %>% mutate(cmod = map(data, crude_mod))
m_list <- by_product %>% mutate(mmod = map(data, mod1))
n_list <- by_product %>% mutate(nn = map(data, samplesize))

crude <- c_list %>% select(-data)%>% unnest(cmod) %>% as_tibble() %>% mutate(p.adjust = p.adjust(p.value, method="BH"))
adjusted <- m_list %>% select(-data)%>% unnest(mmod) %>% as_tibble()%>% mutate(p.adjust = p.adjust(p.value, method="BH"))
nn <- n_list %>% select(-data) %>% unnest(nn) %>% as_tibble() 
min(nn$n);min(nn$n_event);max(nn$n);max(nn$n_event)



# Ovarian cancer --------------------
data1 <- dataOv %>%
  as_tibble()%>%
  select_at(vars(all_of(productname), all_of(covs1), "FU_OvCa_EOFAgeExact_ooph","AgeExact_Baseline","FU_OvCa_Event"))%>%
  mutate_at(vars(-BMI, -PA, -AgeExact_Baseline, -FU_OvCa_EOFAgeExact_ooph, -FU_OvCa_Event, -all_of(productname)), factor)%>%
  filter_at(vars(all_of(covs1)), all_vars(!is.na(.))) 

# create list column 
by_product <- data1 %>%
  pivot_longer(all_of(productname), names_to="product", values_to = "frequency")%>%
  group_by(product) %>% nest()

# Crude model 
crude_mod <-  function(df) {
  df %>%
    coxph(Surv(AgeExact_Baseline, FU_OvCa_EOFAgeExact_ooph, FU_OvCa_Event) ~ frequency, ties='breslow', data=.) %>% 
    broom::tidy(.,exponentiate = TRUE, conf.int= TRUE) %>%
    select(term, estimate, std.error, conf.low, conf.high, p.value)
}

# Main model  
covs1
mod1 <-  function(df) {
  df %>%
    coxph(Surv(AgeExact_Baseline, FU_OvCa_EOFAgeExact_ooph, FU_OvCa_Event) ~ frequency  + race + educ + income + menopause*rcs(BMI, quantile(BMI, c(0, .5, .35, .65, .95, 1), na.rm = TRUE)) + 
            smoking + PA + BCduration + HRT1 + alcohol , ties='breslow', data=.) %>% 
    broom::tidy(.,exponentiate = TRUE, conf.int= TRUE) %>%
    select(term, estimate, std.error, conf.low, conf.high, p.value)
}


c_list <- by_product %>% mutate(cmod = map(data, crude_mod))
m_list <- by_product %>% mutate(mmod = map(data, mod1))

crude <- c_list %>% select(-data)%>% unnest(cmod) %>% as_tibble() %>% mutate(p.adjust = p.adjust(p.value, method="BH"))
adjusted <- m_list %>% select(-data)%>% unnest(mmod) %>% as_tibble()%>% mutate(p.adjust = p.adjust(p.value, method="BH"))


samplesize <- function(df){
  df %>% filter_at(vars(frequency, race, educ, income, menopause, BMI, smoking, PA, BCduration, HRT1, alcohol), all_vars(!is.na(.))) %>% summarise(n=nrow(.), n_event=sum(FU_OvCa_Event))
}
n_list <- by_product %>% mutate(nn = map(data, samplesize))
nn <- n_list %>% select(-data) %>% unnest(nn) %>% as_tibble() 
min(nn$n);min(nn$n_event);max(nn$n);max(nn$n_event)

# Uterine cancer --------------------

data1 <- dataUE %>%
  as_tibble()%>%
  select_at(vars(all_of(productname), all_of(covs2), "FU_UECa_EOFAgeExact_hyst","AgeExact_Baseline","FU_UECa_Event"))%>%
  mutate_at(vars(-BMI, -PA, -AgeExact_Baseline, -FU_UECa_EOFAgeExact_hyst, -FU_UECa_Event, -all_of(productname)), factor)%>%
  filter_at(vars(all_of(covs2)), all_vars(!is.na(.))) 


# create list column 
by_product <- data1 %>%
  pivot_longer(all_of(productname), names_to="product", values_to = "frequency")%>%
  group_by(product) %>% nest()

# Crude model 
crude_mod <-  function(df) {
  df %>%
    coxph(Surv(AgeExact_Baseline, FU_UECa_EOFAgeExact_hyst, FU_UECa_Event) ~ frequency, ties='breslow', data=.) %>% 
    broom::tidy(.,exponentiate = TRUE, conf.int= TRUE) %>%
    select(term, estimate, std.error, conf.low, conf.high, p.value)
}

# Main model  
covs1
mod1 <-  function(df) {
  df %>%
    coxph(Surv(AgeExact_Baseline, FU_UECa_EOFAgeExact_hyst, FU_UECa_Event) ~ frequency  + race + educ + income + menopause*rcs(BMI, quantile(BMI, c(0, .5, .35, .65, .95, 1), na.rm = TRUE)) + 
            smoking + PA + BCduration + HRT2 + alcohol , ties='breslow', data=.) %>% 
    broom::tidy(.,exponentiate = TRUE, conf.int= TRUE) %>%
    slice_head(n = 1) %>%
    select(term, estimate, std.error, conf.low, conf.high, p.value)
}




c_list <- by_product %>% mutate(cmod = map(data, crude_mod))
m_list <- by_product %>% mutate(mmod = map(data, mod1))

crude <- c_list %>% select(-data)%>% unnest(cmod) %>% as_tibble() %>% mutate(p.adjust = p.adjust(p.value, method="BH"))
adjusted <- m_list %>% select(-data)%>% unnest(mmod) %>% as_tibble()%>% mutate(p.adjust = p.adjust(p.value, method="BH"))


samplesize <- function(df){
  df %>% filter_at(vars(frequency, race, educ, income, menopause, BMI, smoking, PA, BCduration, HRT2, alcohol), all_vars(!is.na(.))) %>% summarise(n=nrow(.), n_event=sum(FU_UECa_Event))
}
n_list <- by_product %>% mutate(nn = map(data, samplesize))
nn <- n_list %>% select(-data) %>% unnest(nn) %>% as_tibble() 
min(nn$n);min(nn$n_event);max(nn$n);max(nn$n_event)

# Missing n for all the analysis -----
# BC -- 
49578
# min: use the smallest n for our mixture model 
1-47088/49578
# max: use the largest n for our single model 
1-48792/49578


# OV --
40610
# min: use the smallest n for our mixture model 
1-38667/40610
# max: use the largest n for our single model 
1-39991/40610

# UE --
33976
# min: use the smallest n for our mixture model 
1-32408/33976
# max: use the largest n for our single model 
1-33477/33976


# pre and post menopause for ovarian cancer analysis 

BCpre <- read_csv("clean_data/BCpre.csv"); BCpost <- read_csv("clean_data/BCpost.csv"); BCmeno <- read_csv("clean_data/BCmeno.csv")
Ovpre <- read_csv("clean_data/Ovpre.csv"); Ovpost <- read_csv("clean_data/Ovpost.csv"); Ovmeno <- read_csv("clean_data/Ovmeno.csv")
UEpre <- read_csv("clean_data/UEpre.csv"); UEpost <- read_csv("clean_data/UEpost.csv"); UEmeno <- read_csv("clean_data/UEmeno.csv")

# Breast Cancer --
predat1 <- BCpre %>%
  filter_at(vars(all_of(covs1),  "tstart", "tstop", "event3"), all_vars(!is.na(.)))%>%
  mutate_at(vars("race","educ","income", "menopause", "smoking", "alcohol", "BCduration", "HRT1"), factor)
postdat1 <- BCpost %>%
  filter_at(vars(all_of(covs1),  "tstart", "tstop", "event3"), all_vars(!is.na(.)))%>%
  mutate_at(vars("race","educ","income", "menopause", "smoking", "alcohol", "BCduration", "HRT1"), factor)
menodat1 <- BCmeno %>%
  filter_at(vars(all_of(covs1),  "tstart", "tstop", "event3"), all_vars(!is.na(.)))%>%
  mutate_at(vars("race","educ","income", "menopause", "smoking", "alcohol", "BCduration", "HRT1"), factor)

# Ovarian Cancer --
predat2 <- Ovpre %>%
  filter_at(vars(all_of(covs1),  "tstart", "tstop", "event3"), all_vars(!is.na(.)))%>%
  mutate_at(vars("race","educ","income", "menopause", "smoking", "alcohol", "BCduration", "HRT1"), factor)
postdat2 <- Ovpost %>%
  filter_at(vars(all_of(covs1),  "tstart", "tstop", "event3"), all_vars(!is.na(.)))%>%
  mutate_at(vars("race","educ","income", "menopause", "smoking", "alcohol", "BCduration", "HRT1"), factor)
menodat2 <- Ovmeno %>%
  filter_at(vars(all_of(covs1),  "tstart", "tstop", "event3"), all_vars(!is.na(.)))%>%
  mutate_at(vars("race","educ","income", "menopause", "smoking", "alcohol", "BCduration", "HRT1"), factor)

# Uterine Cancer --
predat3 <- UEpre %>%
  filter_at(vars(all_of(covs2), "tstart", "tstop", "event3"), all_vars(!is.na(.)))%>%
  mutate_at(vars("race","educ","income", "menopause", "smoking", "alcohol", "BCduration", "HRT2"), factor)
postdat3 <- UEpost %>%
  filter_at(vars(all_of(covs2),  "tstart", "tstop", "event3"), all_vars(!is.na(.)))%>%
  mutate_at(vars("race","educ","income", "menopause", "smoking", "alcohol", "BCduration", "HRT2"), factor)
menodat3 <- UEmeno %>%
  filter_at(vars(all_of(covs2),  "tstart", "tstop", "event3"), all_vars(!is.na(.)))%>%
  mutate_at(vars("race","educ","income", "menopause", "smoking", "alcohol", "BCduration", "HRT2"), factor)


# Ovarian 
pre <- coxph(Surv(tstart, tstop, event3) ~ blush  + race + educ + income + rcs(BMI, quantile(BMI, c(0, .5, .35, .65, .95, 1), na.rm = TRUE)) + 
        smoking + PA + BCduration + HRT1 + alcohol , ties='breslow', data=predat2) %>% 
  broom::tidy(.,exponentiate = TRUE, conf.int= TRUE) %>%
  select(term, estimate, std.error, conf.low, conf.high, p.value)
pre[1,]

post <- coxph(Surv(tstart, tstop, event3) ~ blush  + race + educ + income + rcs(BMI, quantile(BMI, c(0, .5, .35, .65, .95, 1), na.rm = TRUE)) + 
        smoking + PA + BCduration + HRT1 + alcohol , ties='breslow', data=postdat2) %>% 
  broom::tidy(.,exponentiate = TRUE, conf.int= TRUE) %>%
  select(term, estimate, std.error, conf.low, conf.high, p.value)
post[1,]

prepost1 <- coxph(Surv(tstart, tstop, event3) ~ blush  + race + educ + income + rcs(BMI, quantile(BMI, c(0, .5, .35, .65, .95, 1), na.rm = TRUE)) + 
                smoking + PA + BCduration + HRT1 + alcohol + 
                  race:prepost + educ:prepost + income:prepost + rcs(BMI, quantile(BMI, c(0, .5, .35, .65, .95, 1), na.rm = TRUE)):prepost + 
                  smoking:prepost + PA:prepost + BCduration:prepost + HRT1:prepost + alcohol:prepost, ties='breslow', data=menodat2) 


prepost2 <- coxph(Surv(tstart, tstop, event3) ~ blush  + race + educ + income + rcs(BMI, quantile(BMI, c(0, .5, .35, .65, .95, 1), na.rm = TRUE)) + 
                 smoking + PA + BCduration + HRT1 + alcohol + 
                   race:prepost + educ:prepost + income:prepost + rcs(BMI, quantile(BMI, c(0, .5, .35, .65, .95, 1), na.rm = TRUE)):prepost + 
                  smoking:prepost + PA:prepost + BCduration:prepost + HRT1:prepost + alcohol:prepost + blush:prepost , ties='breslow',  data=menodat2) 
round(waldtest(prepost1, prepost2)$`Pr(>Chisq)`[2],4)

# + strata will give us exact the same estimate as stratified analysis, but the p-value for heterogeneity will not be right 
menodat2 %>% mutate(prepost = relevel(as.factor(prepost),ref="0"))%>%
coxph(Surv(tstart, tstop, event3) ~ blush  + race + educ + income + rcs(BMI, quantile(BMI, c(0, .5, .35, .65, .95, 1), na.rm = TRUE)) + 
        smoking + PA + BCduration + HRT1 + alcohol + 
        race:prepost + educ:prepost + income:prepost + rcs(BMI, quantile(BMI, c(0, .5, .35, .65, .95, 1), na.rm = TRUE)):prepost + 
        smoking:prepost + PA:prepost + BCduration:prepost + HRT1:prepost + alcohol:prepost + blush:prepost + strata(prepost), ties='breslow',  data=.) %>%
  broom::tidy(.,exponentiate = TRUE, conf.int= TRUE) %>%
  select(term, estimate, std.error, conf.low, conf.high, p.value) %>% slice_head(n=1)

menodat2 %>% mutate(prepost = relevel(as.factor(prepost),ref="1"))%>%
  coxph(Surv(tstart, tstop, event3) ~ blush  + race + educ + income + rcs(BMI, quantile(BMI, c(0, .5, .35, .65, .95, 1), na.rm = TRUE)) + 
          smoking + PA + BCduration + HRT1 + alcohol + 
          race:prepost + educ:prepost + income:prepost + rcs(BMI, quantile(BMI, c(0, .5, .35, .65, .95, 1), na.rm = TRUE)):prepost + 
          smoking:prepost + PA:prepost + BCduration:prepost + HRT1:prepost + alcohol:prepost + blush:prepost + strata(prepost), ties='breslow',  data=.) %>%
  broom::tidy(.,exponentiate = TRUE, conf.int= TRUE) %>%
  select(term, estimate, std.error, conf.low, conf.high, p.value) %>% slice_head(n=1)


m <-c()
for (i in 1:length(productname)){
prepost1 <- coxph(Surv(tstart, tstop, event3) ~ get(productname[i])  + race + educ + income + rcs(BMI, quantile(BMI, c(0, .5, .35, .65, .95, 1), na.rm = TRUE)) + 
                    smoking + PA + BCduration + HRT1 + alcohol + 
                    race:prepost + educ:prepost + income:prepost + rcs(BMI, quantile(BMI, c(0, .5, .35, .65, .95, 1), na.rm = TRUE)):prepost + 
                    smoking:prepost + PA:prepost + BCduration:prepost + HRT1:prepost + alcohol:prepost, ties='breslow', data=menodat2) 


prepost2 <- coxph(Surv(tstart, tstop, event3) ~ get(productname[i])   + race + educ + income + rcs(BMI, quantile(BMI, c(0, .5, .35, .65, .95, 1), na.rm = TRUE)) + 
                    smoking + PA + BCduration + HRT1 + alcohol + 
                    race:prepost + educ:prepost + income:prepost + rcs(BMI, quantile(BMI, c(0, .5, .35, .65, .95, 1), na.rm = TRUE)):prepost + 
                    smoking:prepost + PA:prepost + BCduration:prepost + HRT1:prepost + alcohol:prepost + get(productname[i]) :prepost , ties='breslow',  data=menodat2) 
m <- rbind(m,round(waldtest(prepost1, prepost2)$`Pr(>Chisq)`[2],4))
}
cbind(productname, m)



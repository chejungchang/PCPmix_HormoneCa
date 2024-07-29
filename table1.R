# Table 1 

library(tidyverse)
library(naniar) # missing data summary and vis 
library(table1)
library(xlsx)

dataBC <- read_csv("clean_data/dataBC.csv") 
dataOv <- read_csv("clean_data/dataOv.csv") 
dataUE <- read_csv("clean_data/dataUE.csv") 

dataBC %>% filter(race==3) %>% select(SE_RACE15) %>% table()/1297*100
dataOv %>% filter(race==3) %>% select(SE_RACE15) %>% table()/(291+75+672+6)*100
dataUE %>% filter(race==3) %>% select(SE_RACE15) %>% table()/(263+63+524+4)*100

# 5 = non-hispanic asian/pacls 
# 7 = non-hispanic american indians 
# 9 = non-hispanic other
# 15 = unknown


table1_BC <- dataBC %>%
  mutate(py = FU_BCInvD_EOFAgeExact-AgeExact_Baseline)%>%
  select(FU_BCInvD_Event, AgeExact_Baseline, py, race, income, educ, ADI, urban, alcohol, smoking, parity_BC, 
         agefirstbirth, menopause, BCduration, HRT1, menarche, breastfeed, PA, BMI, HEI)%>%
  mutate_at(vars(-BMI,-PA, -ADI, -AgeExact_Baseline, -py, -HEI), as.factor)


label(table1_BC$AgeExact_Baseline) <- "Age at baseline (year)"
label(table1_BC$py) <- "Follow-up time (year)"

table1_BC$race <- factor(table1_BC$race, levels = c(1,2,0,3), 
                         labels = c("Black", "Hispanic", "Non-Hispanic White", "Other"))
label(table1_BC$race) <- "Race and ethnicity"

table1_BC$income <- factor(table1_BC$income, level = c(0,1,2), 
                           labels = c("<50,000", "50,000-100,000", ">=100,000"))
label(table1_BC$income) <- "Income"

table1_BC$educ <- factor(table1_BC$educ, level = c(0,1,2), 
                         labels = c("High school or less", "Some college", "College or above"))
label(table1_BC$educ) <- "Education"

label(table1_BC$ADI) <- "Area deprivation index (percentile)"

table1_BC$alcohol <- factor(table1_BC$alcohol, levels = c(0,1,2), 
                            labels = c("Never or past", "Current <1 drink/day", "Current >=1 drinks/day"))
label(table1_BC$alcohol) <-"Alcohol consumption" 

table1_BC$smoking <- factor(table1_BC$smoking, levels = c(0,1), 
                            labels = c("Never or past", "Current"))
label(table1_BC$smoking) <-"Smoking status"

table1_BC$parity_BC <- factor(table1_BC$parity_BC, levels = c(0,1,2), 
                              labels = c("0-1", "2","3"))
label(table1_BC$parity_BC) <-"Parity"

table1_BC$agefirstbirth <- factor(table1_BC$agefirstbirth, levels = c(0,1,2,3), 
                                  labels = c("Nulliparous", "<23","23-27", ">=27"))
label(table1_BC$agefirstbirth) <-"Age at first birth (year)"

table1_BC$menopause <- factor(table1_BC$menopause, levels = c(0,1), 
                              labels = c("Premenopausal", "Postmenopausal"))
label(table1_BC$menopause) <-"Menopausal status at baseline"

table1_BC$BCduration <- factor(table1_BC$BCduration, levels = c(0,1,2,3), 
                               labels = c("None","<2 years", "2-<10 years", ">=10 years"))
label(table1_BC$BCduration) <-"Oral contraceptive use"

table1_BC$HRT1 <- factor(table1_BC$HRT1, levels = c(0,1,2), 
                         labels = c("None", "Estrogen alone", "Estrogen plus Progestin"))
label(table1_BC$HRT1) <-"Hormone replacement therapy use" 

table1_BC$menarche <- factor(table1_BC$menarche, levels = c(0,1), 
                             labels = c("<12", ">=12"))
label(table1_BC$menarche) <-"Age at menarche (year)" 

table1_BC$breastfeed<- factor(table1_BC$breastfeed, levels = c(0,1), 
                              labels = c("<48", ">=48"))
label(table1_BC$breastfeed) <-"Breast feeding duration (month)" 

label(table1_BC$PA) <-"Physical activity (metabolic equivalent MET-hours/week)" 

label(table1_BC$BMI) <-"Body mass index (kg/m2)" 

label(table1_BC$HEI) <-"Healthy eating index" 

table1_BC$urban <- factor(table1_BC$urban, levels=c(1,2,3), labels=c("Urban", "Suburban, small town, other", "Rural"))
label(table1_BC$urban) <- "Urbanicity"

(tableone_BC <- table1(~.|factor(FU_BCInvD_Event), data = table1_BC, overall = T))

table1_Ov <- dataOv %>%
  mutate(py = FU_OvCa_EOFAgeExact_ooph-AgeExact_Baseline)%>%
  select(FU_OvCa_Event, AgeExact_Baseline, py, race, income, educ, ADI, urban, alcohol, smoking, parity_BC, 
         agefirstbirth, menopause, BCduration, HRT1, menarche, breastfeed, PA, BMI, HEI)%>%
  mutate_at(vars(-BMI,-PA, -ADI, -AgeExact_Baseline, -py, -HEI), as.factor)

label(table1_Ov$AgeExact_Baseline) <- "Age at baseline (year)"
label(table1_Ov$py) <- "Follow-up time (year)"

table1_Ov$race <- factor(table1_Ov$race, levels = c(1,2,0,3), 
                         labels = c("Black", "Hispanic", "Non-Hispanic White", "Other"))
label(table1_Ov$race) <- "Race and ethnicity"

table1_Ov$income <- factor(table1_Ov$income, level = c(0,1,2), 
                           labels = c("<50,000", "50,000-100,000", ">=100,000"))
label(table1_Ov$income) <- "Income"

table1_Ov$educ <- factor(table1_Ov$educ, level = c(0,1,2), 
                         labels = c("High school or less", "Some college", "College or above"))
label(table1_Ov$educ) <- "Education"

label(table1_Ov$ADI) <- "Area deprivation index (percentile)"

table1_Ov$alcohol <- factor(table1_Ov$alcohol, levels = c(0,1,2), 
                            labels = c("Never or past", "Current <1 drink/day", "Current >=1 drinks/day"))
label(table1_Ov$alcohol) <-"Alcohol consumption" 

table1_Ov$smoking <- factor(table1_Ov$smoking, levels = c(0,1), 
                            labels = c("Never or past", "Current"))
label(table1_Ov$smoking) <-"Smoking status"

table1_Ov$parity_BC <- factor(table1_Ov$parity_BC, levels = c(0,1,2), 
                              labels = c("0-1", "2","3"))
label(table1_Ov$parity_BC) <-"Parity"

table1_Ov$agefirstbirth <- factor(table1_Ov$agefirstbirth, levels = c(0,1,2,3), 
                                  labels = c("Nulliparous", "<23","23-27", ">=27"))
label(table1_Ov$agefirstbirth) <-"Age at first birth (year)"

table1_Ov$menopause <- factor(table1_Ov$menopause, levels = c(0,1), 
                              labels = c("Premenopausal", "Postmenopausal"))
label(table1_Ov$menopause) <-"Menopausal status at baseline"

table1_Ov$BCduration <- factor(table1_Ov$BCduration, levels = c(0,1,2,3), 
                               labels = c("None","<2 years", "2-<10 years", ">=10 years"))
label(table1_Ov$BCduration) <-"Oral contraceptive use"

table1_Ov$HRT1 <- factor(table1_Ov$HRT1, levels = c(0,1,2), 
                         labels = c("None", "Estrogen alone", "Estrogen plus Progestin"))
label(table1_Ov$HRT1) <-"Hormone replacement therapy use" 

table1_Ov$menarche <- factor(table1_Ov$menarche, levels = c(0,1), 
                             labels = c("<12", ">=12"))
label(table1_Ov$menarche) <-"Age at menarche (year)" 

table1_Ov$breastfeed<- factor(table1_Ov$breastfeed, levels = c(0,1), 
                              labels = c("<48", ">=48"))
label(table1_Ov$breastfeed) <-"Breast feeding duration (month)" 

label(table1_Ov$PA) <-"Physical activity (metabolic equivalent MET-hours/week)" 

label(table1_Ov$BMI) <-"Body mass index (kg/m2)" 

label(table1_Ov$HEI) <-"Healthy eating index" 

table1_Ov$urban <- factor(table1_Ov$urban, levels=c(1,2,3), labels=c("Urban", "Suburban, small town, other", "Rural"))
label(table1_Ov$urban) <- "Urbanicity"

(tableone_Ov <- table1(~.|factor(FU_OvCa_Event), data = table1_Ov, overall = T))

table1_UE <- dataUE %>%
  mutate(py = FU_UECa_EOFAgeExact_hyst-AgeExact_Baseline)%>%  
  select(FU_UECa_Event, AgeExact_Baseline, py, race, income, educ, ADI, urban, alcohol, smoking, parity_BC, 
         agefirstbirth, menopause, BCduration, HRT2, menarche, breastfeed, PA, BMI, HEI)%>%
  mutate_at(vars(-BMI,-PA, -ADI, -AgeExact_Baseline, -py, -HEI), as.factor)

label(table1_UE$AgeExact_Baseline) <- "Age at baseline (year)"
label(table1_UE$py) <- "Follow-up time (year)"

table1_UE$race <- factor(table1_UE$race, levels = c(1,2,0,3), 
                         labels = c("Black", "Hispanic", "Non-Hispanic White", "Other"))
label(table1_UE$race) <- "Race and ethnicity"

table1_UE$income <- factor(table1_UE$income, level = c(0,1,2), 
                           labels = c("<50,000", "50,000-100,000", ">=100,000"))
label(table1_UE$income) <- "Income"

table1_UE$educ <- factor(table1_UE$educ, level = c(0,1,2), 
                         labels = c("High school or less", "Some college", "College or above"))
label(table1_UE$educ) <- "Education"

label(table1_UE$ADI) <- "Area deprivation index (percentile)"

table1_UE$alcohol <- factor(table1_UE$alcohol, levels = c(0,1,2), 
                            labels = c("Never or past", "Current <1 drink/day", "Current >=1 drinks/day"))
label(table1_UE$alcohol) <-"Alcohol consumption" 

table1_UE$smoking <- factor(table1_UE$smoking, levels = c(0,1), 
                            labels = c("Never or past", "Current"))
label(table1_UE$smoking) <-"Smoking status"

table1_UE$parity_BC <- factor(table1_UE$parity_BC, levels = c(0,1,2), 
                              labels = c("0-1", "2","3"))
label(table1_UE$parity_BC) <-"Parity"

table1_UE$agefirstbirth <- factor(table1_UE$agefirstbirth, levels = c(0,1,2,3), 
                                  labels = c("Nulliparous", "<23","23-27", ">=27"))
label(table1_UE$agefirstbirth) <-"Age at first birth (year)"

table1_UE$menopause <- factor(table1_UE$menopause, levels = c(0,1), 
                              labels = c("Premenopausal", "Postmenopausal"))
label(table1_UE$menopause) <-"Menopausal status at baseline"

table1_UE$BCduration <- factor(table1_UE$BCduration, levels = c(0,1,2,3), 
                               labels = c("None","<2 years", "2-<10 years", ">=10 years"))
label(table1_UE$BCduration) <-"Oral contraceptive use"

table1_UE$HRT2 <- factor(table1_UE$HRT2, levels = c(0,1,2), 
                         labels = c("None", "Estrogen alone", "Estrogen plus Progestin"))
label(table1_UE$HRT2) <-"Hormone replacement therapy use" 

table1_UE$menarche <- factor(table1_UE$menarche, levels = c(0,1), 
                             labels = c("<12", ">=12"))
label(table1_UE$menarche) <-"Age at menarche (year)" 

table1_UE$breastfeed<- factor(table1_UE$breastfeed, levels = c(0,1), 
                              labels = c("<48", ">=48"))
label(table1_UE$breastfeed) <-"Breast feeding duration (month)" 

label(table1_UE$PA) <-"Physical activity (metabolic equivalent MET-hours/week)" 

label(table1_UE$BMI) <-"Body mass index (kg/m2)" 

label(table1_UE$HEI) <-"Healthy eating index" 

table1_UE$urban <- factor(table1_UE$urban, levels=c(1,2,3), labels=c("Urban", "Suburban, small town, other", "Rural"))
label(table1_UE$urban) <- "Urbanicity"

(tableone_UE <- table1(~.|factor(FU_UECa_Event), data = table1_UE, overall = T))

write.xlsx(tableone_BC, "output/table1.xlsx", sheetName="BC", col.names=TRUE, row.names=TRUE, append=FALSE)
write.xlsx(tableone_Ov, "output/table1.xlsx", sheetName="Ov", col.names=TRUE, row.names=TRUE, append=TRUE)
write.xlsx(tableone_UE, "output/table1.xlsx", sheetName="UE", col.names=TRUE, row.names=TRUE, append=TRUE)



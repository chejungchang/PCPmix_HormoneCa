# Functions 

label_colname <- function(data){
  data = data %>%
    rename("Mascara" = "mascara", 
           "Eye shadow" = "eyeshadow", 
           "Eyeliner" = "eyeliner",
           "Lipstick" = "lipstick",
           "Foundation" = "foundation",
           "Blush" = "blush", 
           "Makeup remover" = "makeupremover", 
           "Perfume" = "perfume", 
           "Nail polish" = "nailpolish", 
           "Nail polish remover" ="nailremover", 
           "Cuticle cream" = "cuticlecream", 
           "Artificial nail" = "artificialnail",
           
           "Lip moisturizer" = "moisturizer", 
           "Cleansing cream" = "cleansing", 
           "Face cream" = "facecream", 
           "Facial mask" = "mask",
           "Anti-aging product" = "antiaging",
           "Age spot lightener" = "agespotlightener", 
           "Blemish product" = "blemish", 
           "Skin lightener" = "skinlightener", 
           "Self-tanner" = "tanner",
           "Baby oil" = "babyoil", 
           "Petroleum jelly" = "petroleumjelly", 
           "Body lotion" = "bodylotion", 
           "Hand lotion" = "handlotion",
           "Foot cream" = "footcream", 
           
           "Deodorant" = "deodorant", 
           "Mouthwash" = "mouthwash", 
           "Bath gel" = "bathgel", 
           "Douche" = "douche", 
           "Talc (under arm)" = "talcarm",              
           "Talc (vaginal)" = "talcvaginal", 
           "Talc (other)" = "talcother",             
           "Shaving cream" = "shavingcream",
           
           "Shampoo" = "shampoo", 
           "Conditioner" = "conditioner", 
           "Hair spray" = "hairspray", 
           "Hair styling product" = "hairgel", 
           "Pomade" = "pomade", 
           "Hair food" = "hairfood", 
           "Minoxidil" = "minoxidil", 
    )
}

# Table 2 ----
## BC function ----

BC_fun <-  function(df) {
  lhs = "Surv(AgeExact_Baseline, FU_BCInvD_EOFAgeExact, FU_BCInvD_Event)"
  ex_string = paste0(ex, collapse=" + ")
  rhs1 = paste0(ex_string)
  crude_form = as.formula(paste(lhs,"~",rhs1))
  
  rhs2 = paste0("race+educ+income+menopause*rcs(BMI, quantile(BMI, c(0, .5, .35, .65, .95, 1), na.rm = TRUE))+smoking+alcohol+BCduration+HRT1+PA+", ex_string)
  adjusted_form = as.formula(paste(lhs,"~",rhs2))
  
  
  mod1 <- qgcomp.cox.noboot(crude_form, expnms = ex, q=NULL, data = df)
  mod2 <- qgcomp.cox.noboot(adjusted_form, expnms = ex, q=NULL, data = df)
  
  a <- tibble(
    n = c(mod1$fit$n, mod2$fit$n), 
    n.event = c(mod1$fit$nevent, mod2$fit$nevent),
    py = c(sum(df$FU_BCInvD_EOFAgeExact - df$AgeExact_Baseline), sum(df$FU_BCInvD_EOFAgeExact - df$AgeExact_Baseline)),
    estimate = c(exp(mod1$psi), exp(mod2$psi)),
    lCI = c(exp(mod1$ci)[1], exp(mod2$ci)[1]),
    uCI = c(exp(mod1$ci)[2], exp(mod2$ci)[2]),
    p.value = c(mod1$pval, mod2$pval), 
    model = c("crude", "adjusted"))
  
  x1 <- data.frame(c(mod1$pos.weights, -mod1$neg.weights))%>%rownames_to_column()
  x1 <- x1[match(productname, x1[,1]),2]
  
  x2 <- data.frame(c(mod2$pos.weights, -mod2$neg.weights))%>%rownames_to_column()
  x2 <- x2[match(productname, x2[,1]),2]
  
  x1.1 <- mod1$fit$coef%>%data.frame()
  x1.1 <- x1.1[match(productname,rownames(x1.1)),1]
  
  x2.1 <- mod2$fit$coef%>%data.frame()
  x2.1 <- x2.1[match(productname,rownames(x2.1)),1]
  
  x1.2 <- coef(summary(mod1$fit))[,3]%>%data.frame()
  x1.2 <- x1.2[match(productname,rownames(x1.2)),1]
  
  x2.2 <- coef(summary(mod2$fit))[,3]%>%data.frame()
  x2.2 <- x2.2[match(productname,rownames(x2.2)),1]
  
  
  b <- tibble(cbind(a, hh=rbind(t(x1),t(x2)), dd=rbind(t(x1.1), t(x2.1)),  qq=rbind(t(x1.2), t(x2.2)))) 
  colnames(b)[9:42] <- paste0(productname, "_weight")
  colnames(b)[43:76] <- paste0(productname, "_coef")
  colnames(b)[77:110] <- paste0(productname, "_se")
  
  print(b)
}

meno_fun <-  function(df) {
  lhs = "Surv(tstart, tstop, event3)"
  ex_string = paste0(ex, collapse=" + ")
  rhs1 = paste0(ex_string)
  crude_form = as.formula(paste(lhs,"~",rhs1))
  
  rhs2 = paste0("race+educ+income+rcs(BMI, quantile(BMI, c(0, .5, .35, .65, .95, 1), na.rm = TRUE))+smoking+alcohol+BCduration+HRT1+PA+",ex_string)
  adjusted_form = as.formula(paste(lhs,"~",rhs2))
  
  mod1 <- qgcomp.cox.noboot(crude_form, expnms = ex, q=NULL, data = df)
  mod2 <- qgcomp.cox.noboot(adjusted_form, expnms = ex, q=NULL, data = df)
  
  a <- tibble(
    n = c(mod1$fit$n, mod2$fit$n), 
    n.event = c(mod1$fit$nevent, mod2$fit$nevent),
    py = c(sum(df$tstop - df$tstart), sum(df$tstop - df$tstart)),
    estimate = c(exp(mod1$psi), exp(mod2$psi)),
    lCI = c(exp(mod1$ci)[1], exp(mod2$ci)[1]),
    uCI = c(exp(mod1$ci)[2], exp(mod2$ci)[2]),
    p.value = c(mod1$pval, mod2$pval), 
    model = c("crude", "adjusted"))
  
  x1 <- data.frame(c(mod1$pos.weights, -mod1$neg.weights))%>%rownames_to_column()
  x1 <- x1[match(productname, x1[,1]),2]
  
  x2 <- data.frame(c(mod2$pos.weights, -mod2$neg.weights))%>%rownames_to_column()
  x2 <- x2[match(productname, x2[,1]),2]
  
  x1.1 <- mod1$fit$coef%>%data.frame()
  x1.1 <- x1.1[match(productname,rownames(x1.1)),1]
  
  x2.1 <- mod2$fit$coef%>%data.frame()
  x2.1 <- x2.1[match(productname,rownames(x2.1)),1]
  
  x1.2<- coef(summary(mod1$fit))[,3]%>%data.frame()
  x1.2 <- x1.2[match(productname,rownames(x1.2)),1]
  
  x2.2<- coef(summary(mod2$fit))[,3]%>%data.frame()
  x2.2 <- x2.2[match(productname,rownames(x2.2)),1]
  
  b <- tibble(cbind(a, hh=rbind(t(x1),t(x2)), dd=rbind(t(x1.1), t(x2.1)),  qq=rbind(t(x1.2), t(x2.2)))) 
  colnames(b)[9:42] <- paste0(productname, "_weight")
  colnames(b)[43:76] <- paste0(productname, "_coef")
  colnames(b)[77:110] <- paste0(productname, "_se")
  
  print(b)
  
}

# meno_pinter_fun <-  function(df) {
#   lhs = "Surv(tstart, tstop, event3)"
#   ex_string = paste0(ex, collapse=" + ")
#   ex_string2 = paste0(ex, collapse=":prepost+ ")
#   
#   rhs1 = paste0("race+educ+income+rcs(BMI, quantile(BMI, c(0, .5, .35, .65, .95, 1), na.rm = TRUE))+smoking+PA+BCduration+alcohol+HRT1+
#                  race:prepost+educ:prepost+income:prepost+rcs(BMI, quantile(BMI, c(0, .5, .35, .65, .95, 1), na.rm = TRUE)):prepost+
#                 smoking:prepost+PA:prepost+BCduration:prepost+alcohol:prepost+HRT1:prepost+", ex_string)
#   rhs2 = paste0("race+educ+income+rcs(BMI, quantile(BMI, c(0, .5, .35, .65, .95, 1), na.rm = TRUE))+smoking+PA+BCduration+alcohol+HRT1+
#                 race:prepost+educ:prepost+income:prepost+rcs(BMI, quantile(BMI, c(0, .5, .35, .65, .95, 1), na.rm = TRUE)):prepost+
#                 smoking:prepost+PA:prepost+BCduration:prepost+alcohol:prepost+HRT1:prepost+",ex_string,"+", ex_string2, ":prepost")
#   
#   reduced = as.formula(paste(lhs,"~",rhs1))
#   full = as.formula(paste(lhs,"~",rhs2))
#   
#   mod1 <- qgcomp.cox.noboot(reduced, expnms = ex, q=NULL, data = df)
#   mod2 <- qgcomp.cox.noboot(full, expnms = ex, q=NULL, data = df)
#   
#   p1 <- round(waldtest(mod1$fit, mod2$fit)$`Pr(>Chisq)`[2],4)
#   
#   print(p1)
# }

meno_pinter_fun <-  function(df) {
  
  df2 <- df %>% as.data.frame() 
  lhs = "Surv(tstart, tstop, event3)"
  ex_string = paste0(ex, collapse=" + ")
  rhs2 = paste0("race+educ+income+rcs(BMI, quantile(BMI, c(0, .5, .35, .65, .95, 1), na.rm = TRUE))+smoking+alcohol+BCduration+HRT1+PA+
                race:prepost+educ:prepost+income:prepost+rcs(BMI, quantile(BMI, c(0, .5, .35, .65, .95, 1), na.rm = TRUE)):prepost+smoking:prepost+alcohol:prepost+BCduration:prepost+HRT1:prepost+PA:prepost+",ex_string)
  adjusted_form = as.formula(paste(lhs,"~",rhs2))
  
  mod2 <- qgcomp.emm.cox.noboot(adjusted_form, expnms = ex, 
                                emmvar="prepost", q=NULL, data = df2)
  print(mod2$pval)
  
}
# 
# meno_pinter_fun <-  function(df) {
#   
#   df2 <- df %>% as.data.frame() 
#   lhs = "Surv(tstart, tstop, event3)"
#   ex_string = paste0(ex, collapse=" + ")
#   rhs2 = paste0("race+educ+income+rcs(BMI, quantile(BMI, c(0, .5, .35, .65, .95, 1), na.rm = TRUE))+smoking+alcohol+BCduration+HRT1+PA")
#   adjusted_form = as.formula(paste(lhs,"~",rhs2))
#   
#   mod2 <- qgcomp.emm.cox.noboot(adjusted_form, expnms = ex, 
#                                 emmvar="prepost", q=NULL, data = df2)
#   print(mod2$pval)
#   
# }


sub_pinter_fun <-  function(df) {
  
  df2 <- df %>% as.data.frame() 
  lhs = "Surv(tstart, tstop, event3)"
  ex_string = paste0(ex, collapse=" + ")
  rhs2 = paste0("race+educ+income+rcs(BMI, quantile(BMI, c(0, .5, .35, .65, .95, 1), na.rm = TRUE))*menopause+smoking+alcohol+BCduration+HRT1+PA+
                race:dat+educ:dat+income:dat+rcs(BMI, quantile(BMI, c(0, .5, .35, .65, .95, 1), na.rm = TRUE)):dat+menopause:dat+rcs(BMI, quantile(BMI, c(0, .5, .35, .65, .95, 1), na.rm = TRUE)):menopause:dat+smoking:dat+alcohol:dat+BCduration:dat+HRT1:dat+PA:dat+",ex_string)
  adjusted_form = as.formula(paste(lhs,"~",rhs2))
  
  mod2 <- qgcomp.emm.cox.noboot(adjusted_form, expnms = ex, 
                                emmvar="dat", q=NULL, data = df2)
  print(mod2$pval)
  
}


## Ov function ----

Ov_fun <-  function(df) {
  lhs = "Surv(AgeExact_Baseline, FU_OvCa_EOFAgeExact_ooph, FU_OvCa_Event)"
  ex_string = paste0(ex, collapse=" + ")
  rhs1 = paste0(ex_string)
  crude_form = as.formula(paste(lhs,"~",rhs1))
  
  rhs2 = paste0("race+educ+income+menopause*rcs(BMI, quantile(BMI, c(0, .5, .35, .65, .95, 1), na.rm = TRUE))+smoking+alcohol+BCduration+HRT1+PA+", ex_string)
  adjusted_form = as.formula(paste(lhs,"~",rhs2))
  
  
  mod1 <- qgcomp.cox.noboot(crude_form, expnms = ex, q=NULL, data = df)
  mod2 <- qgcomp.cox.noboot(adjusted_form, expnms = ex, q=NULL, data = df)
  
  a <- tibble(
    n = c(mod1$fit$n, mod2$fit$n), 
    n.event = c(mod1$fit$nevent, mod2$fit$nevent),
    py = c(sum(df$FU_OvCa_EOFAgeExact_ooph - df$AgeExact_Baseline), sum(df$FU_OvCa_EOFAgeExact_ooph - df$AgeExact_Baseline)),
    estimate = c(exp(mod1$psi), exp(mod2$psi)),
    lCI = c(exp(mod1$ci)[1], exp(mod2$ci)[1]),
    uCI = c(exp(mod1$ci)[2], exp(mod2$ci)[2]),
    p.value = c(mod1$pval, mod2$pval), 
    model = c("crude", "adjusted"))
  
  x1 <- data.frame(c(mod1$pos.weights, -mod1$neg.weights))%>%rownames_to_column()
  x1 <- x1[match(productname, x1[,1]),2]
  
  x2 <- data.frame(c(mod2$pos.weights, -mod2$neg.weights))%>%rownames_to_column()
  x2 <- x2[match(productname, x2[,1]),2]
  
  x1.1 <- mod1$fit$coef%>%data.frame()
  x1.1 <- x1.1[match(productname,rownames(x1.1)),1]
  
  x2.1 <- mod2$fit$coef%>%data.frame()
  x2.1 <- x2.1[match(productname,rownames(x2.1)),1]
  
  x1.2 <- coef(summary(mod1$fit))[,3]%>%data.frame()
  x1.2 <- x1.2[match(productname,rownames(x1.2)),1]
  
  x2.2 <- coef(summary(mod2$fit))[,3]%>%data.frame()
  x2.2 <- x2.2[match(productname,rownames(x2.2)),1]
  
  
  b <- tibble(cbind(a, hh=rbind(t(x1),t(x2)), dd=rbind(t(x1.1), t(x2.1)),  qq=rbind(t(x1.2), t(x2.2)))) 
  colnames(b)[9:42] <- paste0(productname, "_weight")
  colnames(b)[43:76] <- paste0(productname, "_coef")
  colnames(b)[77:110] <- paste0(productname, "_se")
  
  print(b)
}


## UE function ----

UE_fun <-  function(df) {
  lhs = "Surv(AgeExact_Baseline, FU_UECa_EOFAgeExact_hyst, FU_UECa_Event)"
  ex_string = paste0(ex, collapse=" + ")
  rhs1 = paste0(ex_string)
  crude_form = as.formula(paste(lhs,"~",rhs1))
  
  rhs2 = paste0("race+educ+income+menopause*rcs(BMI, quantile(BMI, c(0, .5, .35, .65, .95, 1), na.rm = TRUE))+smoking+alcohol+BCduration+HRT2+PA+", ex_string)
  adjusted_form = as.formula(paste(lhs,"~",rhs2))
  
  
  mod1 <- qgcomp.cox.noboot(crude_form, expnms = ex, q=NULL, data = df)
  mod2 <- qgcomp.cox.noboot(adjusted_form, expnms = ex, q=NULL, data = df)
  
  a <- tibble(
    n = c(mod1$fit$n, mod2$fit$n), 
    n.event = c(mod1$fit$nevent, mod2$fit$nevent),
    estimate = c(exp(mod1$psi), exp(mod2$psi)),
    py = c(sum(df$FU_UECa_EOFAgeExact_hyst - df$AgeExact_Baseline), sum(df$FU_UECa_EOFAgeExact_hyst - df$AgeExact_Baseline)),
    lCI = c(exp(mod1$ci)[1], exp(mod2$ci)[1]),
    uCI = c(exp(mod1$ci)[2], exp(mod2$ci)[2]),
    p.value = c(mod1$pval, mod2$pval), 
    model = c("crude", "adjusted"))
  
  x1 <- data.frame(c(mod1$pos.weights, -mod1$neg.weights))%>%rownames_to_column()
  x1 <- x1[match(productname, x1[,1]),2]
  
  x2 <- data.frame(c(mod2$pos.weights, -mod2$neg.weights))%>%rownames_to_column()
  x2 <- x2[match(productname, x2[,1]),2]
  
  x1.1 <- mod1$fit$coef%>%data.frame()
  x1.1 <- x1.1[match(productname,rownames(x1.1)),1]
  
  x2.1 <- mod2$fit$coef%>%data.frame()
  x2.1 <- x2.1[match(productname,rownames(x2.1)),1]
  
  x1.2 <- coef(summary(mod1$fit))[,3]%>%data.frame()
  x1.2 <- x1.2[match(productname,rownames(x1.2)),1]
  
  x2.2 <- coef(summary(mod2$fit))[,3]%>%data.frame()
  x2.2 <- x2.2[match(productname,rownames(x2.2)),1]
  
  
  b <- tibble(cbind(a, hh=rbind(t(x1),t(x2)), dd=rbind(t(x1.1), t(x2.1)),  qq=rbind(t(x1.2), t(x2.2)))) 
  colnames(b)[9:42] <- paste0(productname, "_weight")
  colnames(b)[43:76] <- paste0(productname, "_coef")
  colnames(b)[77:110] <- paste0(productname, "_se")
  
  
  print(b)
}

UE_meno_fun <-  function(df) {
  lhs = "Surv(tstart, tstop, event3)"
  ex_string = paste0(ex, collapse=" + ")
  rhs1 = paste0(ex_string)
  crude_form = as.formula(paste(lhs,"~",rhs1))
  
  rhs2 = paste0("race+educ+income+rcs(BMI, quantile(BMI, c(0, .5, .35, .65, .95, 1), na.rm = TRUE))+smoking+alcohol+BCduration+HRT2+PA+",ex_string)
  adjusted_form = as.formula(paste(lhs,"~",rhs2))
  
  mod1 <- qgcomp.cox.noboot(crude_form, expnms = ex, q=NULL, data = df)
  mod2 <- qgcomp.cox.noboot(adjusted_form, expnms = ex, q=NULL, data = df)
  
  a <- tibble(
    n = c(mod1$fit$n, mod2$fit$n), 
    n.event = c(mod1$fit$nevent, mod2$fit$nevent),
    py = c(sum(df$tstop - df$tstart), sum(df$tstop - df$tstart)),
    estimate = c(exp(mod1$psi), exp(mod2$psi)),
    lCI = c(exp(mod1$ci)[1], exp(mod2$ci)[1]),
    uCI = c(exp(mod1$ci)[2], exp(mod2$ci)[2]),
    p.value = c(mod1$pval, mod2$pval), 
    model = c("crude", "adjusted"))
  
  x1 <- data.frame(c(mod1$pos.weights, -mod1$neg.weights))%>%rownames_to_column()
  x1 <- x1[match(productname, x1[,1]),2]
  
  x2 <- data.frame(c(mod2$pos.weights, -mod2$neg.weights))%>%rownames_to_column()
  x2 <- x2[match(productname, x2[,1]),2]
  
  x1.1 <- mod1$fit$coef%>%data.frame()
  x1.1 <- x1.1[match(productname,rownames(x1.1)),1]
  
  x2.1 <- mod2$fit$coef%>%data.frame()
  x2.1 <- x2.1[match(productname,rownames(x2.1)),1]
  
  x1.2<- coef(summary(mod1$fit))[,3]%>%data.frame()
  x1.2 <- x1.2[match(productname,rownames(x1.2)),1]
  
  x2.2<- coef(summary(mod2$fit))[,3]%>%data.frame()
  x2.2 <- x2.2[match(productname,rownames(x2.2)),1]
  
  b <- tibble(cbind(a, hh=rbind(t(x1),t(x2)), dd=rbind(t(x1.1), t(x2.1)),  qq=rbind(t(x1.2), t(x2.2)))) 
  colnames(b)[9:42] <- paste0(productname, "_weight")
  colnames(b)[43:76] <- paste0(productname, "_coef")
  colnames(b)[77:110] <- paste0(productname, "_se")
  
  
  print(b)
  
}

# UE_meno_pinter_fun <-  function(df) {
#   lhs = "Surv(tstart, tstop, event3)"
#   ex_string = paste0(ex, collapse=" + ")
#   ex_string2 = paste0(ex, collapse=":prepost+ ")
#   
#   rhs1 = paste0("race+educ+income+rcs(BMI, quantile(BMI, c(0, .5, .35, .65, .95, 1), na.rm = TRUE))+smoking+PA+BCduration+alcohol+HRT2+
#                  race:prepost+educ:prepost+income:prepost+rcs(BMI, quantile(BMI, c(0, .5, .35, .65, .95, 1), na.rm = TRUE)):prepost+
#                 smoking:prepost+PA:prepost+BCduration:prepost+alcohol:prepost+HRT2:prepost+", ex_string)
#   rhs2 = paste0("race+educ+income+rcs(BMI, quantile(BMI, c(0, .5, .35, .65, .95, 1), na.rm = TRUE))+smoking+PA+BCduration+alcohol+HRT2+
#                 race:prepost+educ:prepost+income:prepost+rcs(BMI, quantile(BMI, c(0, .5, .35, .65, .95, 1), na.rm = TRUE)):prepost+
#                 smoking:prepost+PA:prepost+BCduration:prepost+alcohol:prepost+HRT2:prepost+",ex_string,"+", ex_string2, ":prepost ")
#   
#   reduced = as.formula(paste(lhs,"~",rhs1))
#   full = as.formula(paste(lhs,"~",rhs2))
#   
#   mod1 <- qgcomp.cox.noboot(reduced, expnms = ex, q=NULL, data = df)
#   mod2 <- qgcomp.cox.noboot(full, expnms = ex, q=NULL, data = df)
#   
#   p1 <- round(waldtest(mod1$fit, mod2$fit)$`Pr(>Chisq)`[2],4)
#   
#   print(p1)
# }


UE_meno_pinter_fun <-  function(df) {
  
  df2 <- df %>% as.data.frame() 
  lhs = "Surv(tstart, tstop, event3)"
  ex_string = paste0(ex, collapse=" + ")
  rhs2 = paste0("race+educ+income+rcs(BMI, quantile(BMI, c(0, .5, .35, .65, .95, 1), na.rm = TRUE))+smoking+alcohol+BCduration+HRT2+PA+
                race:prepost+educ:prepost+income:prepost+rcs(BMI, quantile(BMI, c(0, .5, .35, .65, .95, 1), na.rm = TRUE)):prepost+smoking:prepost+alcohol:prepost+BCduration:prepost+HRT2:prepost+PA:prepost+",ex_string)
  adjusted_form = as.formula(paste(lhs,"~",rhs2))
  
  mod2 <- qgcomp.emm.cox.noboot(adjusted_form, expnms = ex, 
                                emmvar="prepost", q=NULL, data = df2)
  print(mod2$pval)
  
}




# Table 3 & 4 ----
## BC function ----
BC_race_fun <-  function(df) {
  lhs = "Surv(AgeExact_Baseline, FU_BCInvD_EOFAgeExact, FU_BCInvD_Event)"
  ex_string = paste0(ex, collapse=" + ")
  rhs1 = paste0("(",ex_string,")*race")
  crude_form = as.formula(paste(lhs,"~",rhs1))
  
  rhs2 = paste0("educ+income+menopause*rcs(BMI, quantile(BMI, c(0, .5, .35, .65, .95, 1), na.rm = TRUE))+smoking+alcohol+BCduration+HRT1+PA+race+","(",ex_string,")*race")
  adjusted_form = as.formula(paste(lhs,"~",rhs2))
  
  
  mod1 <- qgcomp.cox.noboot(crude_form, expnms = ex, q=NULL, data = df)
  mod2 <- qgcomp.cox.noboot(adjusted_form, expnms = ex, q=NULL, data = df)
  
  
  a <- tibble(
    estimate = c(exp(mod1$psi), exp(mod2$psi)),
    lCI = c(exp(mod1$ci)[1], exp(mod2$ci)[1]),
    uCI = c(exp(mod1$ci)[2], exp(mod2$ci)[2]),
    p.value = c(mod1$pval, mod2$pval), 
    model = c("crude", "adjusted"))
  
  x1<- data.frame(c(mod1$pos.weights, -mod1$neg.weights))%>%rownames_to_column()
  x1 <- x1[match(productname, x1[,1]),2]
  
  x2<- data.frame(c(mod2$pos.weights, -mod2$neg.weights))%>%rownames_to_column()
  x2<- x2[match(productname, x2[,1]),2]
  
  x1.1<- mod1$fit$coef%>%data.frame()
  x1.1 <- x1.1[match(productname,rownames(x1.1)),1]
  
  x2.1<- mod2$fit$coef%>%data.frame()
  x2.1 <- x2.1[match(productname,rownames(x2.1)),1]
  
  x1.2<- coef(summary(mod1$fit))[,3]%>%data.frame()
  x1.2 <- x1.2[match(productname,rownames(x1.2)),1]
  
  x2.2<- coef(summary(mod2$fit))[,3]%>%data.frame()
  x2.2 <- x2.2[match(productname,rownames(x2.2)),1]
  
  b <- tibble(cbind(a, hh=rbind(t(x1),t(x2)), dd=rbind(t(x1.1), t(x2.1)),  qq=rbind(t(x1.2), t(x2.2)))) 
  colnames(b)[6:39] <- paste0(productname, "_weight")
  colnames(b)[40:73] <- paste0(productname, "_coef")
  colnames(b)[74:107] <- paste0(productname, "_se")
  
  print(b)
}




BC_race_pinter_fun <-  function(df) {
  
  df2 <- df %>% as.data.frame() 
  lhs = "Surv(AgeExact_Baseline, FU_BCInvD_EOFAgeExact, FU_BCInvD_Event)"
  ex_string = paste0(ex, collapse=" + ")
  rhs2 = paste0("educ+income+menopause*rcs(BMI, quantile(BMI, c(0, .5, .35, .65, .95, 1), na.rm = TRUE))+smoking+alcohol+BCduration+HRT1+PA+",ex_string)
  adjusted_form = as.formula(paste(lhs,"~",rhs2))
  
  mod2 <- qgcomp.emm.cox.noboot(adjusted_form, expnms = ex, 
                                emmvar="race", q=NULL, data = df2)
  print(mod2$pval)
  
}

BC_3BMI_fun <-  function(df) {
  lhs = "Surv(AgeExact_Baseline, FU_BCInvD_EOFAgeExact, FU_BCInvD_Event)"
  ex_string = paste0(ex, collapse=" + ")
  rhs1 = paste0("BMI_3c+","(",ex_string,")*BMI_3c")
  crude_form = as.formula(paste(lhs,"~",rhs1))
  rhs2 = paste0("race+educ+income+menopause+smoking+alcohol+BCduration+HRT1+PA+BMI_3c+","(",ex_string,")*BMI_3c")
  adjusted_form = as.formula(paste(lhs,"~",rhs2))
  
  mod1 <- qgcomp.cox.noboot(crude_form, expnms = ex, q=NULL, data = df)
  mod2 <- qgcomp.cox.noboot(adjusted_form, expnms = ex, q=NULL, data = df)
  
  a <- tibble(
    estimate = c(exp(mod1$psi), exp(mod2$psi)),
    lCI = c(exp(mod1$ci)[1], exp(mod2$ci)[1]),
    uCI = c(exp(mod1$ci)[2], exp(mod2$ci)[2]),
    p.value = c(mod1$pval, mod2$pval), 
    model = c("crude", "adjusted"))
  
  x1<- data.frame(c(mod1$pos.weights, -mod1$neg.weights))%>%rownames_to_column()
  x1 <- x1[match(productname, x1[,1]),2]
  
  x2<- data.frame(c(mod2$pos.weights, -mod2$neg.weights))%>%rownames_to_column()
  x2<- x2[match(productname, x2[,1]),2]
  
  x1.1<- mod1$fit$coef%>%data.frame()
  x1.1 <- x1.1[match(productname,rownames(x1.1)),1]
  
  x2.1<- mod2$fit$coef%>%data.frame()
  x2.1 <- x2.1[match(productname,rownames(x2.1)),1]
  
  x1.2<- coef(summary(mod1$fit))[,3]%>%data.frame()
  x1.2 <- x1.2[match(productname,rownames(x1.2)),1]
  
  x2.2<- coef(summary(mod2$fit))[,3]%>%data.frame()
  x2.2 <- x2.2[match(productname,rownames(x2.2)),1]
  
  b <- tibble(cbind(a, hh=rbind(t(x1),t(x2)), dd=rbind(t(x1.1), t(x2.1)),  qq=rbind(t(x1.2), t(x2.2)))) 
  colnames(b)[6:39] <- paste0(productname, "_weight")
  colnames(b)[40:73] <- paste0(productname, "_coef")
  colnames(b)[74:107] <- paste0(productname, "_se")
  
  print(b)
  
}


BC_3BMI_pinter_fun <-  function(df) {
  
  lhs = "Surv(AgeExact_Baseline, FU_BCInvD_EOFAgeExact, FU_BCInvD_Event)"
  ex_string = paste0(ex, collapse=" + ")
  rhs1 = paste0("race+educ+income+menopause+smoking+PA+BCduration+alcohol+HRT1+BMI_3c+",ex_string)
  rhs2 = paste0("race+educ+income+menopause+smoking+PA+BCduration+alcohol+HRT1+(",ex_string,")*BMI_3c")
  reduced = as.formula(paste(lhs,"~",rhs1))
  full = as.formula(paste(lhs,"~",rhs2))
  
  mod1 <- qgcomp.cox.noboot(reduced, expnms = ex, q=NULL, data = df)
  mod2 <- qgcomp.cox.noboot(full, expnms = ex, q=NULL, data = df)
  p2<-round(waldtest(mod1$fit, mod2$fit)$`Pr(>Chisq)`[2],4)
  
  print(p2)
}

## Ov function ----
Ov_race_fun <-  function(df) {
  lhs = "Surv(AgeExact_Baseline, FU_OvCa_EOFAgeExact_ooph, FU_OvCa_Event)"
  ex_string = paste0(ex, collapse=" + ")
  rhs1 = paste0("race","(",ex_string,")*race")
  crude_form = as.formula(paste(lhs,"~",rhs1))
  
  rhs2 = paste0("educ+income+menopause*rcs(BMI, quantile(BMI, c(0, .5, .35, .65, .95, 1), na.rm = TRUE))+smoking+alcohol+BCduration+HRT1+PA+race+","(",ex_string,")*race")
  adjusted_form = as.formula(paste(lhs,"~",rhs2))
  
  mod1 <- qgcomp.cox.noboot(crude_form, expnms = ex, q=NULL, data = df)
  mod2 <- qgcomp.cox.noboot(adjusted_form, expnms = ex, q=NULL, data = df)
  
  a <- tibble(
    estimate = c(exp(mod1$psi), exp(mod2$psi)),
    lCI = c(exp(mod1$ci)[1], exp(mod2$ci)[1]),
    uCI = c(exp(mod1$ci)[2], exp(mod2$ci)[2]),
    p.value = c(mod1$pval, mod2$pval), 
    model = c("crude", "adjusted"))
  
  x1<- data.frame(c(mod1$pos.weights, -mod1$neg.weights))%>%rownames_to_column()
  x1 <- x1[match(productname, x1[,1]),2]
  
  x2<- data.frame(c(mod2$pos.weights, -mod2$neg.weights))%>%rownames_to_column()
  x2<- x2[match(productname, x2[,1]),2]
  
  x1.1<- mod1$fit$coef%>%data.frame()
  x1.1 <- x1.1[match(productname,rownames(x1.1)),1]
  
  x2.1<- mod2$fit$coef%>%data.frame()
  x2.1 <- x2.1[match(productname,rownames(x2.1)),1]
  
  x1.2<- coef(summary(mod1$fit))[,3]%>%data.frame()
  x1.2 <- x1.2[match(productname,rownames(x1.2)),1]
  
  x2.2<- coef(summary(mod2$fit))[,3]%>%data.frame()
  x2.2 <- x2.2[match(productname,rownames(x2.2)),1]
  
  b <- tibble(cbind(a, hh=rbind(t(x1),t(x2)), dd=rbind(t(x1.1), t(x2.1)),  qq=rbind(t(x1.2), t(x2.2)))) 
  colnames(b)[6:39] <- paste0(productname, "_weight")
  colnames(b)[40:73] <- paste0(productname, "_coef")
  colnames(b)[74:107] <- paste0(productname, "_se")
  
  print(b)
}

Ov_race_pinter_fun <-  function(df) {
  
  df2 <- df %>% as.data.frame() 
  lhs = "Surv(AgeExact_Baseline, FU_OvCa_EOFAgeExact_ooph, FU_OvCa_Event)"
  ex_string = paste0(ex, collapse=" + ")
  rhs2 = paste0("educ+income+menopause*rcs(BMI, quantile(BMI, c(0, .5, .35, .65, .95, 1), na.rm = TRUE))+smoking+alcohol+BCduration+HRT1+PA+",ex_string)
  adjusted_form = as.formula(paste(lhs,"~",rhs2))
  
  mod2 <- qgcomp.emm.cox.noboot(adjusted_form, expnms = ex, 
                                emmvar="race", q=NULL, data = df2)
  print(mod2$pval)
  
}


Ov_3BMI_fun <-  function(df) {
  lhs = "Surv(AgeExact_Baseline, FU_OvCa_EOFAgeExact_ooph, FU_OvCa_Event)"
  ex_string = paste0(ex, collapse=" + ")
  rhs1 = paste0("BMI_3c+","(",ex_string,")*BMI_3c")
  crude_form = as.formula(paste(lhs,"~",rhs1))
  rhs2 = paste0("race+educ+income+menopause+smoking+alcohol+BCduration+HRT1+PA+BMI_3c+","(",ex_string,")*BMI_3c")
  adjusted_form = as.formula(paste(lhs,"~",rhs2))
  
  mod1 <- qgcomp.cox.noboot(crude_form, expnms = ex, q=NULL, data = df)
  mod2 <- qgcomp.cox.noboot(adjusted_form, expnms = ex, q=NULL, data = df)
  
  a <- tibble(
    estimate = c(exp(mod1$psi), exp(mod2$psi)),
    lCI = c(exp(mod1$ci)[1], exp(mod2$ci)[1]),
    uCI = c(exp(mod1$ci)[2], exp(mod2$ci)[2]),
    p.value = c(mod1$pval, mod2$pval), 
    model = c("crude", "adjusted"))
  
  x1<- data.frame(c(mod1$pos.weights, -mod1$neg.weights))%>%rownames_to_column()
  x1 <- x1[match(productname, x1[,1]),2]
  
  x2<- data.frame(c(mod2$pos.weights, -mod2$neg.weights))%>%rownames_to_column()
  x2<- x2[match(productname, x2[,1]),2]
  
  x1.1<- mod1$fit$coef%>%data.frame()
  x1.1 <- x1.1[match(productname,rownames(x1.1)),1]
  
  x2.1<- mod2$fit$coef%>%data.frame()
  x2.1 <- x2.1[match(productname,rownames(x2.1)),1]
  
  x1.2<- coef(summary(mod1$fit))[,3]%>%data.frame()
  x1.2 <- x1.2[match(productname,rownames(x1.2)),1]
  
  x2.2<- coef(summary(mod2$fit))[,3]%>%data.frame()
  x2.2 <- x2.2[match(productname,rownames(x2.2)),1]
  
  b <- tibble(cbind(a, hh=rbind(t(x1),t(x2)), dd=rbind(t(x1.1), t(x2.1)),  qq=rbind(t(x1.2), t(x2.2)))) 
  colnames(b)[6:39] <- paste0(productname, "_weight")
  colnames(b)[40:73] <- paste0(productname, "_coef")
  colnames(b)[74:107] <- paste0(productname, "_se")
  
  print(b)
  
}

Ov_3BMI_pinter_fun <-  function(df) {
  
  lhs = "Surv(AgeExact_Baseline, FU_OvCa_EOFAgeExact_ooph, FU_OvCa_Event)"
  ex_string = paste0(ex, collapse=" + ")
  rhs1 = paste0("race+educ+income+menopause+smoking+PA+BCduration+alcohol+HRT1+BMI_3c+",ex_string)
  rhs2 = paste0("race+educ+income+menopause+smoking+PA+BCduration+alcohol+HRT1+BMI_3c+(",ex_string,")*BMI_3c")
  reduced = as.formula(paste(lhs,"~",rhs1))
  full = as.formula(paste(lhs,"~",rhs2))
  
  mod1 <- qgcomp.cox.noboot(reduced, expnms = ex, q=NULL, data = df)
  mod2 <- qgcomp.cox.noboot(full, expnms = ex, q=NULL, data = df)
  p2<-round(waldtest(mod1$fit, mod2$fit)$`Pr(>Chisq)`[2],4)
  
  print(p2)
}

## UE function ----
UE_race_fun <-  function(df) {
  lhs = "Surv(AgeExact_Baseline, FU_UECa_EOFAgeExact_hyst, FU_UECa_Event)"
  ex_string = paste0(ex, collapse=" + ")
  rhs1 = paste0("race+","(",ex_string,")*race")
  crude_form = as.formula(paste(lhs,"~",rhs1))
  
  rhs2 = paste0("educ+income+menopause*rcs(BMI, quantile(BMI, c(0, .5, .35, .65, .95, 1), na.rm = TRUE))+smoking+alcohol+BCduration+HRT2+PA+race+","(",ex_string,")*race")
  adjusted_form = as.formula(paste(lhs,"~",rhs2))
  
  mod1 <- qgcomp.cox.noboot(crude_form, expnms = ex, q=NULL, data = df)
  mod2 <- qgcomp.cox.noboot(adjusted_form, expnms = ex, q=NULL, data = df)
  
  a <- tibble(
    estimate = c(exp(mod1$psi), exp(mod2$psi)),
    lCI = c(exp(mod1$ci)[1], exp(mod2$ci)[1]),
    uCI = c(exp(mod1$ci)[2], exp(mod2$ci)[2]),
    p.value = c(mod1$pval, mod2$pval), 
    model = c("crude", "adjusted"))
  
  x1<- data.frame(c(mod1$pos.weights, -mod1$neg.weights))%>%rownames_to_column()
  x1 <- x1[match(productname, x1[,1]),2]
  
  x2<- data.frame(c(mod2$pos.weights, -mod2$neg.weights))%>%rownames_to_column()
  x2<- x2[match(productname, x2[,1]),2]
  
  x1.1<- mod1$fit$coef%>%data.frame()
  x1.1 <- x1.1[match(productname,rownames(x1.1)),1]
  
  x2.1<- mod2$fit$coef%>%data.frame()
  x2.1 <- x2.1[match(productname,rownames(x2.1)),1]
  
  x1.2<- coef(summary(mod1$fit))[,3]%>%data.frame()
  x1.2 <- x1.2[match(productname,rownames(x1.2)),1]
  
  x2.2<- coef(summary(mod2$fit))[,3]%>%data.frame()
  x2.2 <- x2.2[match(productname,rownames(x2.2)),1]
  
  b <- tibble(cbind(a, hh=rbind(t(x1),t(x2)), dd=rbind(t(x1.1), t(x2.1)),  qq=rbind(t(x1.2), t(x2.2)))) 
  colnames(b)[6:39] <- paste0(productname, "_weight")
  colnames(b)[40:73] <- paste0(productname, "_coef")
  colnames(b)[74:107] <- paste0(productname, "_se")
  
  print(b)
}


UE_race_pinter_fun <-  function(df) {
  
  df2 <- df %>% as.data.frame() 
  lhs = "Surv(AgeExact_Baseline, FU_UECa_EOFAgeExact_hyst, FU_UECa_Event)"
  ex_string = paste0(ex, collapse=" + ")
  rhs2 = paste0("educ+income+menopause*rcs(BMI, quantile(BMI, c(0, .5, .35, .65, .95, 1), na.rm = TRUE))+smoking+alcohol+BCduration+HRT2+PA+",ex_string)
  adjusted_form = as.formula(paste(lhs,"~",rhs2))
  
  mod2 <- qgcomp.emm.cox.noboot(adjusted_form, expnms = ex, 
                                emmvar="race", q=NULL, data = df2)
  print(mod2$pval)
  
}

UE_3BMI_fun <-  function(df) {
  lhs = "Surv(AgeExact_Baseline, FU_UECa_EOFAgeExact_hyst, FU_UECa_Event)"
  ex_string = paste0(ex, collapse=" + ")
  rhs1 = paste0("BMI_3c+","(",ex_string,")*BMI_3c")
  crude_form = as.formula(paste(lhs,"~",rhs1))
  rhs2 = paste0("race+educ+income+menopause+smoking+alcohol+BCduration+HRT2+PA+BMI_3c+","(",ex_string,")*BMI_3c")
  adjusted_form = as.formula(paste(lhs,"~",rhs2))
  
  mod1 <- qgcomp.cox.noboot(crude_form, expnms = ex, q=NULL, data = df)
  mod2 <- qgcomp.cox.noboot(adjusted_form, expnms = ex, q=NULL, data = df)
  
  a <- tibble(
    estimate = c(exp(mod1$psi), exp(mod2$psi)),
    lCI = c(exp(mod1$ci)[1], exp(mod2$ci)[1]),
    uCI = c(exp(mod1$ci)[2], exp(mod2$ci)[2]),
    p.value = c(mod1$pval, mod2$pval), 
    model = c("crude", "adjusted"))
  
  x1<- data.frame(c(mod1$pos.weights, -mod1$neg.weights))%>%rownames_to_column()
  x1 <- x1[match(productname, x1[,1]),2]
  
  x2<- data.frame(c(mod2$pos.weights, -mod2$neg.weights))%>%rownames_to_column()
  x2<- x2[match(productname, x2[,1]),2]
  
  x1.1<- mod1$fit$coef%>%data.frame()
  x1.1 <- x1.1[match(productname,rownames(x1.1)),1]
  
  x2.1<- mod2$fit$coef%>%data.frame()
  x2.1 <- x2.1[match(productname,rownames(x2.1)),1]
  
  x1.2<- coef(summary(mod1$fit))[,3]%>%data.frame()
  x1.2 <- x1.2[match(productname,rownames(x1.2)),1]
  
  x2.2<- coef(summary(mod2$fit))[,3]%>%data.frame()
  x2.2 <- x2.2[match(productname,rownames(x2.2)),1]
  
  b <- tibble(cbind(a, hh=rbind(t(x1),t(x2)), dd=rbind(t(x1.1), t(x2.1)),  qq=rbind(t(x1.2), t(x2.2)))) 
  colnames(b)[6:39] <- paste0(productname, "_weight")
  colnames(b)[40:73] <- paste0(productname, "_coef")
  colnames(b)[74:107] <- paste0(productname, "_se")
  
  print(b)
  
}

UE_3BMI_pinter_fun <-  function(df) {
  
  lhs = "Surv(AgeExact_Baseline, FU_UECa_EOFAgeExact_hyst, FU_UECa_Event)"
  ex_string = paste0(ex, collapse=" + ")
  rhs1 = paste0("race+educ+income+menopause+smoking+PA+BCduration+alcohol+HRT2+BMI_3c+",ex_string)
  rhs2 = paste0("race+educ+income+menopause+smoking+PA+BCduration+alcohol+HRT2+BMI_3c+(",ex_string,")*BMI_3c")
  reduced = as.formula(paste(lhs,"~",rhs1))
  full = as.formula(paste(lhs,"~",rhs2))
  
  mod1 <- qgcomp.cox.noboot(reduced, expnms = ex, q=NULL, data = df)
  mod2 <- qgcomp.cox.noboot(full, expnms = ex, q=NULL, data = df)
  p2<-round(waldtest(mod1$fit, mod2$fit)$`Pr(>Chisq)`[2],4)
  
  print(p2)
}


# P-value for table 1 ----- 
pvalue <- function(x, ...) {
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # For numeric variables, perform a standard 2-sample t-test
    p <- t.test(y ~ g)$p.value
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g))$p.value
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}



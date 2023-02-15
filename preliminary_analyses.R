# rm(list=ls())

# libraries 
library(lavaan)
library(readxl)
library(tidyverse)
library(rstatix)
library(corrplot)
library(psych)

# read the data
db_pm <- read_excel("data_pm.xlsx", sheet = 1)
db_gr <- read_excel('data_pm.xlsx',sheet=2)


# preprocessing and aggregation of the datasets
dbpm <- db_pm %>% # disaggregated dataset
  filter(esclusi_1 == 0) %>% # exclude some outliers
  mutate(RT = ifelse(is.na(RT), 0, RT)) %>% # replace RT's NA with 0
  group_by(id_sogg) %>% # group by subject
  dplyr::summarise(pm_rrs = sum(Corrette)/sum(RT)*1e3) %>% # create PM_RRS variable
  mutate(pm_rrs = ifelse(is.nan(pm_rrs),0,pm_rrs)) %>% # replace 0/0 with 0
  left_join(db_gr, by = 'id_sogg') %>% # join with the aggregated database
  mutate(single_rrs = ong30_c/ong30_RT*1e3/32) %>% # 32 is the number of trials 
  mutate(dual_rrs = ong33_c/ong33_RT*1e3/32) %>% # and RT is the average RT
  dplyr::select(c(id_sogg,pm_rrs,Gruppo,SpanRedux,gng_acc,
                  IE_etero,AFV_etero,Voti,Cp_etero,
                  IE_auto,AFV_auto,Cp_auto, 
                  single_rrs, dual_rrs,
                  ong30acc,ong30_RT,ong33_acc,ong33_RT,
                  prosp_acc,prospRT,RT_no_m_out)) %>% # select just the columns i need
  mutate(gng_acc = gng_acc*10/3) %>% # rescale gng_acc (Go NoGo task) in [0-100]
  filter(!is.na(Gruppo)) # remove the row with missing value in group

# rename the columns
names(dbpm)[c(2:12,15:21)] <- c('PM','Age','WM_span','Go_NoGo','EI_etero',
                                'PVA_etero','Grades','PSB_etero',
                                'EI_auto','PVA_auto','PSB_auto',
                                'single_acc','single_rt','dual_acc','dual_rt',
                                'pm_acc', 'pm_rt', 'gonogo_rt')




# 2 samples t-tests and Cohen's d

# single ot accuracy
t.test(dbpm$single_acc~dbpm$Age,var.equal=TRUE)
sd(dbpm$single_acc[dbpm$Age==1],na.rm=TRUE)
sd(dbpm$single_acc[dbpm$Age==2],na.rm=TRUE)
cohen.d(dbpm$single_acc, dbpm$Age) 

# single ot rt
t.test(dbpm$single_rt~dbpm$Age,var.equal=TRUE)
sd(dbpm$single_rt[dbpm$Age==1],na.rm=TRUE)
sd(dbpm$single_rt[dbpm$Age==2],na.rm=TRUE)
cohen.d(dbpm$single_rt, dbpm$Age)

# single ot rrs
t.test(dbpm$single_rrs~dbpm$Age,var.equal=TRUE)
sd(dbpm$single_rrs[dbpm$Age==1],na.rm=TRUE)
sd(dbpm$single_rrs[dbpm$Age==2],na.rm=TRUE)
cohen.d(dbpm$single_rrs, dbpm$Age)

# dual ot accuracy
t.test(dbpm$dual_acc~dbpm$Age,var.equal=TRUE)
sd(dbpm$dual_acc[dbpm$Age==1],na.rm=TRUE)
sd(dbpm$dual_acc[dbpm$Age==2],na.rm=TRUE)
cohen.d(dbpm$dual_acc, dbpm$Age)

# dual ot RT
t.test(dbpm$dual_rt~dbpm$Age,var.equal=TRUE)
sd(dbpm$dual_rt[dbpm$Age==1],na.rm=TRUE)
sd(dbpm$dual_rt[dbpm$Age==2],na.rm=TRUE)
cohen.d(dbpm$dual_rt, dbpm$Age)

# dual ot rrs
t.test(dbpm$dual_rrs~dbpm$Age,var.equal=TRUE)
sd(dbpm$dual_rrs[dbpm$Age==1],na.rm=TRUE)
sd(dbpm$dual_rrs[dbpm$Age==2],na.rm=TRUE)
cohen.d(dbpm$dual_rrs, dbpm$Age)

# pm accuracy
t.test(dbpm$pm_acc~dbpm$Age,var.equal=TRUE)
sd(dbpm$pm_acc[dbpm$Age==1],na.rm=TRUE)
sd(dbpm$pm_acc[dbpm$Age==2],na.rm=TRUE)
cohen.d(dbpm$pm_acc, dbpm$Age)

# pm rt
t.test(dbpm$pm_rt~dbpm$Age,var.equal=TRUE)
sd(dbpm$pm_rt[dbpm$Age==1],na.rm=TRUE)
sd(dbpm$pm_rt[dbpm$Age==2],na.rm=TRUE)
cohen.d(dbpm$pm_rt, dbpm$Age)

# pm rrs
t.test(dbpm$PM~dbpm$Age,var.equal=TRUE)
sd(dbpm$PM[dbpm$Age==1],na.rm=TRUE)
sd(dbpm$PM[dbpm$Age==2],na.rm=TRUE)
cohen.d(dbpm$PM, dbpm$Age)

# WM
t.test(dbpm$WM_span~dbpm$Age,var.equal=TRUE)
sd(dbpm$WM_span[dbpm$Age==1],na.rm=TRUE)
sd(dbpm$WM_span[dbpm$Age==2],na.rm=TRUE)
cohen.d(dbpm$WM_span, dbpm$Age)

# Go-NoGo accuracy
t.test(dbpm$Go_NoGo~dbpm$Age,var.equal=TRUE)
sd(dbpm$Go_NoGo[dbpm$Age==1],na.rm=TRUE)
sd(dbpm$Go_NoGo[dbpm$Age==2],na.rm=TRUE)
cohen.d(dbpm$Go_NoGo, dbpm$Age)

# Go-NoGo RT
t.test(dbpm$gonogo_rt~dbpm$Age,var.equal=TRUE)
sd(dbpm$gonogo_rt[dbpm$Age==1],na.rm=TRUE)
sd(dbpm$gonogo_rt[dbpm$Age==2],na.rm=TRUE)
cohen.d(dbpm$gonogo_rt, dbpm$Age)

# Grades
t.test(dbpm$Grades~dbpm$Age,var.equal=TRUE)
sd(dbpm$Grades[dbpm$Age==1],na.rm=TRUE)
sd(dbpm$Grades[dbpm$Age==2],na.rm=TRUE)
cohen.d(dbpm$Grades, dbpm$Age)


# # EI self
# t.test(dbpm$EI_auto~dbpm$Age,var.equal=TRUE)
# sd(dbpm$EI_auto[dbpm$Age==1],na.rm=TRUE)
# sd(dbpm$EI_auto[dbpm$Age==2],na.rm=TRUE)
# cohen.d(dbpm$EI_auto, dbpm$Age)
# 
# # PVA self
# t.test(dbpm$PVA_auto~dbpm$Age,var.equal=TRUE)
# sd(dbpm$PVA_auto[dbpm$Age==1],na.rm=TRUE)
# sd(dbpm$PVA_auto[dbpm$Age==2],na.rm=TRUE)
# cohen.d(dbpm$PVA_auto, dbpm$Age)
# 
# # CP self
# t.test(dbpm$PSB_auto~dbpm$Age,var.equal=TRUE)
# sd(dbpm$PSB_auto[dbpm$Age==1],na.rm=TRUE)
# sd(dbpm$PSB_auto[dbpm$Age==2],na.rm=TRUE)
# cohen.d(dbpm$PSB_auto, dbpm$Age)

# EI teacher
t.test(dbpm$EI_etero~dbpm$Age,var.equal=TRUE)
sd(dbpm$EI_etero[dbpm$Age==1],na.rm=TRUE)
sd(dbpm$EI_etero[dbpm$Age==2],na.rm=TRUE)
cohen.d(dbpm$EI_etero, dbpm$Age)


# PVA teacher
t.test(dbpm$PVA_etero~dbpm$Age,var.equal=TRUE)
sd(dbpm$PVA_etero[dbpm$Age==1],na.rm=TRUE)
sd(dbpm$PVA_etero[dbpm$Age==2],na.rm=TRUE)
cohen.d(dbpm$PVA_etero, dbpm$Age)


# CP teacher
t.test(dbpm$PSB_etero~dbpm$Age,var.equal=TRUE)
sd(dbpm$PSB_etero[dbpm$Age==1],na.rm=TRUE)
sd(dbpm$PSB_etero[dbpm$Age==2],na.rm=TRUE)
cohen.d(dbpm$PSB_etero, dbpm$Age)



# ANOVAs

db_aov <- data.frame(EI = c(dbpm$EI_auto,dbpm$EI_etero),
                     PVA = c(dbpm$PVA_auto,dbpm$PVA_etero),
                     PSB = c(dbpm$PSB_auto,dbpm$PSB_etero),
                     Age = as.factor(rep(dbpm$Age, 2)),
                     Evaluation = as.factor(c(rep('self', 157),rep('teacher', 157))),
                     ID = rep(dbpm$id_sogg, 2))



# EI
anova_test(data = db_aov, 
           dv = EI,
           wid = ID,
           within = Evaluation,
           between = Age,
           effect.size = 'pes')


TukeyHSD(aov(EI ~ Evaluation + Age:Evaluation, data=db_aov))


# PVA
anova_test(data = db_aov, 
           dv = PVA,
           wid = ID,
           within = Evaluation,
           between = Age,
           effect.size = 'pes')

TukeyHSD(aov(PVA ~ Evaluation + Age:Evaluation, data=db_aov))



# CP
anova_test(data = db_aov, 
           dv = PSB,
           wid = ID,
           within = Evaluation,
           between = Age,
           effect.size = 'pes')

TukeyHSD(aov(PSB ~ Evaluation, data=db_aov))



# correlations

db_cor <- data.frame(dbpm$single_rrs, dbpm$dual_rrs, dbpm$PM,
                     dbpm$Grades, dbpm$WM_span, dbpm$Go_NoGo,
                     dbpm$gonogo_rt, dbpm$EI_auto, dbpm$PVA_auto,
                     dbpm$PSB_auto, dbpm$EI_etero, dbpm$PVA_etero, dbpm$PSB_etero,
                     dbpm$Age)

names(db_cor) <- c('Single_OT', 'Dual_OT', 'PM', 'Grades',
                   'WM_span', 'Go_NoGo', 'Go_NoGo_RT',
                   'EI_self','PVA_self','PSB_self',
                   'EI_teacher','PVA_teacher','PSB_teacher',
                   'Age')


# function for the computation of the correlation's pvalues
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}


# 8 y.o.

db_cor_8 <- db_cor %>% 
  filter(Age == 1) %>%
  dplyr::select(-Age) %>%
  droplevels()

# use = 'pairs' to handle missing values
cor_mat_8 <- round(cor(db_cor_8, use = 'pair'), 2)



# matrix of the p-value of the correlation
# with Benjamini & Yekutieli correction 
# (for the control of the false discovery rate
# in multiple testing under dependency)
p_mat_8 <- cor.mtest(db_cor_8) %>%
  apply(2, p.adjust, method = 'BY')
  
  
coll <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

corrplot(cor_mat_8, method="color", col = coll(200),
         type="lower",  
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, # Text label color and rotation
         p.mat = p_mat_8, sig.level = 0.05, # Combine with significance
        # insig = "blank", # non significant correlations are blank
         mar=c(0,0,1,0), # adjust title
         title = 'Group 1: 8-year-old-children',
         diag=FALSE) # hide correlation coefficient on the principal diagonal



# 12 y.o.

db_cor_12 <- db_cor %>% 
  filter(Age == 2) %>%
  dplyr::select(-Age) %>%
  droplevels()

# use = 'pairs' to handle missing values
cor_mat_12 <- round(cor(db_cor_12, use = 'pair'), 2)



# matrix of the p-value of the correlation
# with Benjamini & Yekutieli correction 
# (for the control of the false discovery rate
# in multiple testing under dependency)
p_mat_12 <- cor.mtest(db_cor_12) %>%
  apply(2, p.adjust, method = 'BY')



corrplot(cor_mat_12, method="color", col = coll(200),
         type="lower",  
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, # Text label color and rotation
         p.mat = p_mat_12, sig.level = 0.05, # Combine with significance
         insig = "blank", # non significant correlations are blank
         diag=FALSE, # hide correlation coefficient on the principal diagonal
         mar=c(0,0,1,0), # adjust title
         title = 'Group 2: 12-year-old-children') 













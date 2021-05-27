
##########################################################################################################
                         ### Script Language profile preschoolers with 22q11DS - Everaert/Selten  ### 
##########################################################################################################

## NNA = Nog niet af ##


#####
# Hier alle variabelen beschrijven
# AW = Norm score for Expressive Vocabulary from the CELF Preschool-2-NL (range 0-15)
# AW.r = Raw score for EV
# etc.
####

# Clear workspace:
rm(list=ls())

## load packages
library(readxl)
library(writexl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr) 
library(rstatix)
library(e1071)
library(tidyverse)
library(pastecs)
library(expss)
library(ppcor)
library(broom)
#library(beeswarm)
library(effectsize)

## set working directory
setwd("C:/Users/Emma/Documents/Thuis werken/good-enough-project-template/data/processed") # veranderen naar waar data opgeslagen is

## load data
data <- read_excel("Datafile_repo.xlsx")


### inverting LogoV scores 0-4 to 5-1 (LogoV 0 = new 5, 1 = 4, 2 = 3, 3 = 2, 4 = 1)

## Label                                  Original         New
# > Normal                                   0              5
# > Different from other children's speech,
#    but not enough to cause comment         1              4
# > Different enough to provoke comment, 
#    but possible to understand most speech  2              3
# > Only just intelligible to strangers      3              2
# > Impossible to understand                 4              1

data$LogoV <- 5-data$LogoV
head(data)
str(data)

#write_xlsx(tidydataIndexP,"C:/Users/Emma/Documents/Thuis werken/Testrepo3T.xlsx")

##########################################################################################################
                                        ### Dataframes ###
##########################################################################################################

# Seperate dataframes for indeces, subtests an/or LogoV
dataIndexP <- data[c("Subject", "PPVT", "KS", "RTI", "ETI")]
dataIndex <- data[c("Subject", "KS", "RTI", "ETI")]
dataERTI <- data[c("Subject", "RTI", "ETI")]
dataSubtest <- data[c("Subject", "ZB", "AW", "ZH", "WS", "BAV", "EC", "WC.R")]
dataLogoV <- data[c("Subject", "LogoV")]
dataIndexLogoV <- data[c("Subject", "PPVT", "KS", "RTI", "ETI", "LogoV")]
IndexPdata <- data[c("Subject","AgeMd", "Sex", "SES", "LogoV", "KS", "ETI", "RTI", "PPVT")]

# Long format
tidydataSubtest <- dataSubtest %>% gather(Task, Score, -c(Subject))
tidydataIndexP <- dataIndexP %>% gather(Index, Score, -c(Subject))
tidydataIndex <- dataIndex %>% gather(Index, Score, -c(Subject))
tidydataLogoV <- dataLogoV %>% gather(Schaal, Score, -c(Subject))
tidydataIndexLogoV <- dataIndexLogoV %>% gather(Index, Score, -c(Subject, LogoV))
tidydataERTI <- dataIndexP %>% gather(Index, Score, -c(Subject))

# Complete data (no NA/missing data) > in deze datasets zitten alleen kinderen die voor iedere variabele een score hebben
IndexPdata_comp <- IndexPdata[complete.cases(IndexPdata), ]
tidydataIndexP_comp <- tidydataIndexP[complete.cases(tidydataIndexP), ]
dataERTI_comp <- dataERTI[complete.cases(dataERTI), ]

##########################################################################################################
                                    ### Demographics Table 1 ###
##########################################################################################################
mean(data$AgeMd)
sd(data$AgeMd)
range(data$AgeMd)
table(data$Sex)  # 1 = female, 2 = male
mean(data$SES)
sd(data$SES)
range(data$SES)
sum(is.na(data$IQ)) 
mean(data$IQ, na.rm = TRUE)
sd(data$IQ, na.rm = TRUE)
range(data$IQ, na.rm = TRUE)

##########################################################################################################
                                       ### Missing data ###
##########################################################################################################

## count missing Index scores
count_if(is.na, data$KS)
count_if(is.na, data$RTI)
count_if(is.na, data$ETI)
count_if(is.na, data$PPVT)

## Number of particpants missing X number of CELF subtests or PPVT
dataTask_Missing <- data[c("Subject", "PPVT", "ZB", "AW", "ZH", "WS", "BAV", "EC", "WC.R")]
dataTask_Missing$NMissing = (count_row_if(dataTask_Missing, criterion = NA) -1)  #-1 because they always miss either EC or WC.R beacuse of age
table(dataTask_Missing$NMissing) #table of number of children missing 0, 1, 2, 3, 4, 5, 6 or 7 tasks

# Dataframe with only participants missing one or more index scores (therefore excluded in analyses considering these index scores)
ExcludedIndexP <- subset(IndexPdata, is.na(ETI) == TRUE | is.na(RTI) == TRUE | is.na(KS) == TRUE | is.na(PPVT) == TRUE)
str(ExcludedIndexP)

#### Check whether participants with missing data differ from those with complete date on age, sex, intelligibility, and parental education

# Age difference included / excluded
#skewness(data$AgeMd, na.rm = TRUE) Nodig? > zo ja apart voor included/excluded
#kurtosis(data$AgeMd, na.rm = TRUE)
#ggqqplot(data$AgeMd)
#hist(data$AgeMd)
#levene_test

wilcox.test(ExcludedIndexP$AgeMd, IndexPdata_comp$AgeMd, conf.int = TRUE)
t.test(ExcludedIndexP$AgeMd, IndexPdata_comp$AgeMd)
mean(ExcludedIndexP$AgeMd)
mean(IndexPdata_comp$AgeMd)
cohens_d(ExcludedIndexP$AgeMd, IndexPdata_comp$AgeMd, var.equal = TRUE, hedges.correction = TRUE)

# Parental education difference included / excluded
#skewness(data$SES, na.rm = TRUE) zie boven
#kurtosis(data$SES, na.rm = TRUE)
#ggqqplot(data$SES)
#hist(data$SES)

wilcox.test(ExcludedIndexP$SES, IndexPdata_comp$SES)
t.test(ExcludedIndexP$SES, IndexPdata_comp$SES, na.rm = TRUE)
mean(ExcludedIndexP$SES, na.rm = TRUE)
mean(IndexPdata_comp$SES, na.rm = TRUE)
cohens_d(ExcludedIndexP$SES, IndexPdata_comp$SES, var.equal = TRUE, hedges.correction = TRUE)

# Intelligibility difference included / excluded
wilcox.test(ExcludedIndexP$LogoV, IndexPdata_comp$LogoV)
t.test(ExcludedIndexP$LogoV, IndexPdata_comp$LogoV, na.rm = TRUE)
mean(ExcludedIndexP$LogoV, na.rm = TRUE)
mean(IndexPdata_comp$LogoV, na.rm = TRUE)
cohens_d(ExcludedIndexP$LogoV, IndexPdata_comp$LogoV, var.equal = TRUE, hedges.correction = TRUE)

# sex difference included / excluded
IndexPdata_Excl<- IndexPdata
IndexPdata_Excl$Missing = (is.na(IndexPdata_Excl$ETI) == TRUE | is.na(IndexPdata_Excl$RTI) == TRUE | is.na(IndexPdata_Excl$KS) == TRUE | is.na(IndexPdata_Excl$PPVT) == TRUE)
chisqtbl <- table(IndexPdata_Excl$Missing, IndexPdata_Excl$Sex)
chisq.test(chisqtbl)
cramers_v(chisqtbl, ci = 0.95, adjust = FALSE)


##########################################################################################################
                                ### Language outcomes ###
##########################################################################################################

## 1st word and sentence
# Parents were asked "How old was your child when it spoke its first word/sentence?"
table(data$FstWord) # 1 = no words yet, 2 = younger than most children (<10 months), 3 = same age (11-15 months), 4 = slightly older (16-24 months), 5 = older (> 25 months)
count_if(gt(3), data$FstWord)
table(data$FstSent) # 1 = no sentences yet, 2 = younger (<18 months), 3 = same age (19-24 months), 4 = slightly older (25-30 months), 5 = older (> 31 months)
SentDelay <- data %>% filter(FstSent > 3 | FstSent == 1)
count(SentDelay)

## Mean, SD, range, etc.
#data$Subject <-as.character(data$Subject)
resIndexP <- stat.desc(dataIndexP)
round(resIndexP, 2)     #limits to 2 decimals
resSubtest <- stat.desc(dataSubtest)
round(resSubtest, 2)

## N index scores below -1 SD
count_if(lt(85), dataIndexP$KS)
count_if(lt(85), dataIndexP$RTI)
count_if(lt(85), dataIndexP$ETI)
count_if(lt(85), dataIndexP$PPVT)

## N index scores above +1 SD
count_if(gt(114), dataIndexP$KS)
count_if(gt(114), dataIndexP$RTI)
count_if(gt(114), dataIndexP$ETI)
count_if(gt(114), dataIndexP$PPVT)

## N index scores below -1,5 SD
count_if(lt(78), dataIndexP$KS)
count_if(lt(78), dataIndexP$RTI)
count_if(lt(78), dataIndexP$ETI)
count_if(lt(78), dataIndexP$PPVT)

## N subtask scores below -1 SD
count_if(lt(7), data$ZB)
count_if(lt(7), data$AW)
count_if(lt(7), data$ZH)
count_if(lt(7), data$WS)
count_if(lt(7), data$BAV)
count_if(lt(7), data$EC)
count_if(lt(7), data$WC.R)

## N subtasks scores above +1 SD
count_if(gt(12), data$ZB)
count_if(gt(12), data$AW)
count_if(gt(12), data$ZH)
count_if(gt(12), data$WS)
count_if(gt(12), data$BAV)
count_if(gt(12), data$EC)
count_if(gt(12), data$WC.R)


##########################################################################################################
                                ### Normality and statistics RTI-ETI ###
##########################################################################################################

## outliers
tidydataIndexP %>%
  group_by(Index) %>%
  identify_outliers(Score)
## For the CELF, we have no reason to assume the score of subject 2119 is not representative or an overestimation of their capabilities
## For the PPVT, we have no reason to assume the score of subject 2123 is not representative or an overestimation of their capabilities

## normality qqplot
ggqqplot(data$KS)
ggqqplot(data$RTI)
ggqqplot(data$ETI)
ggqqplot(data$PPVT)
hist(data$KS)
hist(data$RTI)
hist(data$ETI)
hist(data$PPVT)


#NNA
## normality histogram
ggplot(tidydataIndexP, aes(sample = Score), ggtheme = theme_bw())+stat_qq()+
  facet_wrap(.~Index)

#ggplot(tidydataIndexP, aes(x = Score, y = Index)) +
#  geom_histogram(binwidth = 1) +
#  facet_grid(tidydataIndexP$Index)
#NNA

## Shapiro-Wilk normality    (# alternatief: shapiro.test(dataIndex_comp$KS)#)
tidydataIndexP %>%
  group_by(Index) %>%
  shapiro_test(Score)

## skewness and kurtosis
skewness(data$KS, na.rm = TRUE)
kurtosis(data$KS, na.rm = TRUE)
skewness(data$RTI, na.rm = TRUE)
kurtosis(data$RTI, na.rm = TRUE)
skewness(data$ETI, na.rm = TRUE)
kurtosis(data$ETI, na.rm = TRUE)
skewness(data$PPVT, na.rm = TRUE)
kurtosis(data$PPVT, na.rm = TRUE)

## homogeneity of variance
tidydataERTI %>% levene_test(Score ~ Index)

## t-test
count(dataERTI_comp)

wilcox.test(data$RTI, data$ETI, paired = TRUE, na.rm = TRUE)
t.test(data$RTI, data$ETI, paired = TRUE, na.rm = TRUE)
cohens_d(data$RTI, data$ETI, var.equal = TRUE, paired = TRUE, hedges.correction = TRUE)

## correlation RTI & ETI
cor_test(data, RTI, ETI, method = "pearson", alternative = "two.sided", conf.level = 0.95)
cor_test(data, RTI, ETI, method = "spearman", alternative = "two.sided", conf.level = 0.95)

# Scatter plot RTI vs ETI
ggplot(data, aes(x= RTI, y = ETI))+
  geom_point(alpha = 0.3, na.rm= TRUE) +
  geom_smooth (method = "lm", color = "blue", alpha = 0.5, linetype = "solid", size = 0.5, na.rm= TRUE)


##########################################################################################################
                          ### Relation language and speech outcomes ###
##########################################################################################################

# Mean, SD, frequencies
resLogoV <- stat.desc(dataLogoV)
round(resLogoV, 2)
table(data$LogoV)
count_if(gt(2.5), data$LogoV)

### Correlations ###

## correlation intelligibilty (LogoV) and age (AgeMd)
cor_test(data, LogoV, AgeMd, method = "pearson", alternative = "two.sided", conf.level = 0.95)
cor_test(data, LogoV, AgeMd, method = "spearman", alternative = "two.sided", conf.level = 0.95)
cor_test(data, LogoV, AgeMd, method = "kendall", alternative = "two.sided", conf.level = 0.95)

## correlation intelligibility (LogoV) and language (KS, RTI, ETI, PPVT)

## Pearson
cor_mat(data, RTI, ETI, KS, PPVT, LogoV, method = "pearson", alternative = "two.sided", conf.level = 0.95)
cor_test(data, vars = LogoV, vars2 = c(RTI, ETI, KS, PPVT), method = "pearson", alternative = "two.sided", conf.level = 0.95)
## Spearman
cor_mat(data, RTI, ETI, KS, PPVT, LogoV, method = "spearman", alternative = "two.sided", conf.level = 0.95)
cor_test(data, vars = LogoV, vars2 = c(RTI, ETI, KS, PPVT), method = "spearman", alternative = "two.sided", conf.level = 0.95)
## Kendall
cor_mat(data, RTI, ETI, KS, PPVT, LogoV, method = "kendall", alternative = "two.sided", conf.level = 0.95)
cor_test(data, vars = LogoV, vars2 = c(RTI, ETI, KS, PPVT), method = "kendall", alternative = "two.sided", conf.level = 0.95)

## correlation RTI-ETI difference and LogoV
data$DifRE <- (data$RTI - data$ETI) # Difference between RTI - ETI
cor_test(data, LogoV, DifRE, method = "pearson", alternative = "two.sided", conf.level = 0.95)

### plots ###

# Scatter plot AgeYd vs LogoV
ggplot(data, aes(x= AgeYd, y = LogoV, alpha = 0.8,))+
  geom_point(alpha = 0.3, na.rm= TRUE) +
  geom_smooth (method = "lm", color = "blue", alpha = 0.5, linetype = "solid", size = 0.5, na.rm= TRUE)

# Scatter plot KS vs LogoV
ggplot(data, aes(x= KS, y = LogoV))+
  geom_point(alpha = 0.3, na.rm= TRUE) +
  geom_smooth (method = "lm", color = "blue", alpha = 0.5, linetype = "solid", size = 0.5, na.rm= TRUE)

# Scatter plot RTI vs LogoV
ggplot(data, aes(x= RTI, y = LogoV))+
  geom_point(alpha = 0.3, na.rm= TRUE) +
  geom_smooth (method = "lm", color = "blue", alpha = 0.5, linetype = "solid", size = 0.5, na.rm= TRUE)

# Scatter plot ETI vs LogoV
ggplot(data, aes(x= ETI, y = LogoV))+
  geom_point(alpha = 0.3, position = position_jitter(width = 0.1), na.rm= TRUE) +
  geom_smooth (method = "lm", color = "blue", alpha = 0.5, linetype = "solid", size = 0.5, na.rm= TRUE)

# Scatter plot PPVT vs LogoV
ggplot(data, aes(x= PPVT, y = LogoV))+
  geom_point(alpha = 0.3, na.rm= TRUE) +
  geom_smooth (method = "lm", color = "blue", alpha = 0.5, linetype = "solid", size = 0.5, na.rm= TRUE)


### regression ###

## NNA > participant als factor meenemen?

# welke assumpties?

skewness(data$LogoV, na.rm = TRUE)
kurtosis(data$LogoV, na.rm = TRUE)
ggqqplot(data$LogoV)
hist(data$LogoV)
# normality checks for language variables can be found above

tidydataIndexLogoV %>%
  group_by(LogoV) %>%
  identify_outliers(Score)

tidydataIndexLogoV %>%
  group_by(LogoV) %>%
  Score(~mahalanobis_distance(.)) %>%
  filter(is.outlier == TRUE)


# no multicollinearity because only 1 predictor

# Linear model relation KS-LogoV
ModelKS <- lm(data$KS ~ data$LogoV)
summary(ModelKS)
anova(ModelKS)
confint(ModelKS, 'data$LogoV', level=0.95)


dataIndexLogoV_comp <- dataIndexLogoV[complete.cases(dataIndexLogoV), ] #dataframe without missing values for CELF indices, PPVT and speech (LogoV)
modelKS <- lm(KS ~ LogoV, data = dataIndexLogoV_comp)
modelKS
modelKS.diag.metrics <- augment(modelKS, dataIndexLogoV_comp)
head(modelKS.diag.metrics)
par(mfrow = c(2, 2))
plot(modelKS)
#autoplot(modelKS)
summary(modelKS)
anova(modelKS)

dataIndexLogoV_comp$Subject <-as.character(dataIndexLogoV_comp$Subject)
logdataIndexLogoV_comp <- log(dataIndexLogoV_comp) #log transfrom data
modellogKS <- lm(KS ~ LogoV, data = logdataIndexLogoV_comp)
modellogKS.diag.metrics <- augment(modellogKS, logdataIndexLogoV_comp)
head(modellogKS.diag.metrics)
par(mfrow = c(2, 2))
plot(modellogKS) #outliers 2119, 2127 and 2107?
#log transform maakt het niet echt beter volgens mij

#cooks distance is ok

ModelKS <- lm(KS ~ LogoV, data = dataIndexLogoV)
summary(ModelKS)
anova(ModelKS)
confint(ModelKS, 'LogoV', level=0.95)



#Linear model relation RTI-LogoV
ModelRTI <- lm(data$RTI ~ data$LogoV)
summary(ModelRTI)
anova(ModelRTI)
confint(ModelRTI, 'data$LogoV', level=0.95)

#Linear model relation ETI-LogoV
ModelETI <- lm(data$ETI ~ data$LogoV)
summary(ModelETI)
anova(ModelETI)
confint(ModelETI, 'data$LogoV', level=0.95)

#Linear model relation PPVT-LogoV
ModelPPVT <- lm(data$PPVT ~ data$LogoV)
summary(ModelPPVT)
anova(ModelPPVT)
confint(ModelPPVT, 'data$LogoV', level=0.95)



#Logarithmic model KS-LogoV
LModelKS <- lm(data$KS ~ log(data$LogoV))
summary(LModelKS)
anova(LModelKS)

#Logarithmic model RTI-LogoV
LModelRL <- lm(data$RTI ~ log(data$LogoV))
summary(LModelRL)
anova(LModelRL)

#Logarithmic model ETI-LogoV
LModelETI <- lm(data$ETI ~ log(data$LogoV))
summary(LModelETI)
anova(LModelETI)

#Logarithmic model RTI-LogoV
LModelPPVT <- lm(data$PPVT ~ log(data$LogoV))
summary(LModelPPVT)
anova(LModelPPVT)


##########################################################################################################
          ### Categories: Number of children in categories Language +/- and Speech +/- ###
##########################################################################################################

### NNA UITZOEKEN HOE JE ONDERSTAANDE IN ÉÉN TABEL WEERGEEFT ####

### Receptive language (RTI) vs. Speech (LogoV)
RTI_NP <- dataIndexLogoV %>%  ## No Problems
  filter(RTI > 84 & LogoV > 2.5)
RTI_SP <- dataIndexLogoV %>%  ## Speech Problems
  filter(RTI > 84 & LogoV < 3)
RTI_LP <- dataIndexLogoV %>%  ## Language Problems
  filter(RTI < 85 & LogoV > 2.5)
RTI_SLP <- dataIndexLogoV %>%  ## Speech and Language Problems
  filter(RTI < 85 & LogoV < 3)

#with(dataIndexLogoV, table(RTI, LogoV))

#RTI_NSLP <- data.frame(RTI_NP, RTI_SP, RTI_LP, RTI_SLP)
#table(RTI_NSLP)

### Expressive language (ETI) vs. Speech (LogoV)
ETI_NP <- dataIndexLogoV %>%  ## No Problems
  filter(ETI > 84 & LogoV > 2.5)
ETI_SP <- dataIndexLogoV %>%  ## Speech Problems
  filter(ETI > 84 & LogoV < 3)
ETI_LP <- dataIndexLogoV %>%  ## Language Problems
  filter(ETI < 85 & LogoV > 2.5)
ETI_SLP <- dataIndexLogoV %>%  ## Speech and Language Problems
  filter(ETI < 85 & LogoV < 3)

### Receptive and expressive language (RTI and ETI) vs. Speech (LogoV)
RETI_NP <- dataIndexLogoV %>%  ## No Problems
  filter(RTI > 84 & ETI > 84 & LogoV > 2.5)
RETI_SP <- dataIndexLogoV %>%  ## Speech Problems
  filter(RTI > 84 & ETI > 84 & LogoV < 3)
RETI_LP <- dataIndexLogoV %>%  ## Language Problems
  filter(RTI < 85 & ETI < 85 & LogoV > 2.5)
RETI_SLP <- dataIndexLogoV %>%  ## Speech and Language Problems
  filter(RTI < 85 & ETI < 85 & LogoV < 3)

### Core language (KS) vs. Speech (LogoV)
KS_NP <- dataIndexLogoV %>%  ## No Problems
  filter(KS > 84 & LogoV > 2.5)
KS_SP <- dataIndexLogoV %>%  ## Speech Problems
  filter(KS > 84 & LogoV < 3)
KS_LP <- dataIndexLogoV %>%  ## Language Problems
  filter(KS < 85 & LogoV > 2.5)
KS_SLP <- dataIndexLogoV %>%  ## Speech and Language Problems
  filter(KS < 85 & LogoV < 3)

### Receptive vocabulary (PPVT) vs. Speech (LogoV)
PPVT_NP <- dataIndexLogoV %>%  ## No Problems
  filter(PPVT > 84 & LogoV > 2.5)
PPVT_SP <- dataIndexLogoV %>%  ## Speech Problems
  filter(PPVT > 84 & LogoV < 3)
PPVT_LP <- dataIndexLogoV %>%  ## Language Problems
  filter(PPVT < 85 & LogoV > 2.5)
PPVT_SLP <- dataIndexLogoV %>%  ## Speech and Language Problems
  filter(PPVT < 85 & LogoV < 3)



##########################################################################################################
                                              ### Extra ###
##########################################################################################################


### Number of children with all scores in the normal or the below average range

# N all subtasks in the normal range (8 or higher) 
SubTNormRange <- data %>%  
  filter((ZB > 7 & AW > 7 & ZH > 7 & WS > 7 & BAV > 7  & WC.R > 7) | (ZB > 7 & AW > 7 & ZH > 7 & WS > 7 & BAV > 7 & EC > 7))
count(SubTNormRange)

# N all subtasks in the below average range (-1 SD or more; 7 or lower)
SubTBelowRange <- data %>%  
  filter(ZB < 8 & AW < 8 & ZH < 8 & WS < 8 & BAV < 8 & WC.R < 8 | ZB < 8 & AW < 8 & ZH < 8 & WS < 8 & BAV < 8 & EC < 8)
count(SubTBelowRange)

# N all subtasks in the below average range (-1 SD or more; 7 or lower) including those with missing values
SubTNormRangeNA <- data %>%  
  filter(((ZB < 8  | is.na(ZB)) & (AW < 8  | is.na(AW)) & (ZH < 8  | is.na(ZH)) & (WS < 8  | is.na(WS)) & (BAV < 8  | is.na(BAV)) & (WC.R < 8  | is.na(WC.R))) | ((ZB < 8  | is.na(ZB)) & (AW < 8  | is.na(AW)) & (ZH < 8  | is.na(ZH)) & (WS < 8  | is.na(WS)) & (BAV < 8  | is.na(BAV)) & (EC < 8  | is.na(EC))))
count(SubTNormRangeNA)

# N all indices in the normal range (85 or higher) 
IndexNormRange <- data %>%  
  filter(KS > 84 & RTI > 84 & ETI > 84)
count(IndexNormRange)

# N all indices in the below average range (-1 SD or more; 84 or lower)
IndexBelowRange <- data %>%  
  filter(KS < 85 & RTI < 85 & ETI < 85)
count(IndexBelowRange)

# N all indices in the below average range (-1 SD or more; 84 or lower) including those with missing values
IndexBelowRangeNA <- data %>%  
  filter((KS < 85  | is.na(KS)) & (RTI < 85  | is.na(RTI)) & (ETI < 85  | is.na(ETI)))
count(IndexBelowRangeNA)
 

##########################################################################################################
                                           ### Figures ###
##########################################################################################################


                                        ### Figuur losse subtaken ###
##########################################################################################################

# complete data set # nodig voor plot
descriptivesSubtest <- tidydataSubtest %>%
  group_by(Task) %>%
  summarize(
    sd = sd(Score, na.rm = TRUE),
    Score = mean(Score, na.rm = TRUE),
    upper = Score + sd,
    lower = Score - sd
  ) 

# Indexen ordenen voor x-as plot
tidydataSubtest$Task <- factor(tidydataSubtest$Task,levels = c("WS", "ZH", "AW", "BAV", "ZB", "WC.R", "WC.E", "WC.T", "EC"))
descriptivesSubtest$Task <- factor(descriptivesSubtest$Task,levels = c("WS", "ZH", "AW", "BAV", "ZB", "WC.R", "WC.E", "WC.T", "EC"))


# Plot met losse datapunten, positie Y exact, jitter op X
plotSubtest  <- ggplot(tidydataSubtest, aes(x= Task, y = Score, color = Task, fill = Task))+
  geom_point(alpha = 0.25, position = position_jitter(width = 0.15, height = 0.001), size = 2, na.rm= TRUE) +
  geom_point(data = descriptivesSubtest, size = 3, shape = 21, color = "black", alpha = 0.9) +
  geom_errorbar(data = descriptivesSubtest, aes(ymin= lower, ymax = upper), width = 0.2) +
  geom_hline(yintercept = 7, color = "black", alpha = 0.4, linetype = "dashed") +
  geom_hline(yintercept = 13, color = "black", alpha = 0.4,  linetype = "dashed")


                                 ### Figuur Index en PPVT scores ###
##########################################################################################################

# complete data set # nodig voor plot
descriptivesIndexP <- tidydataIndexP %>%
  group_by(Index) %>%
  summarize(
    sd = sd(Score, na.rm = TRUE),
    Score = mean(Score, na.rm = TRUE),
    upper = Score + sd,
    lower = Score - sd
  ) 

# Indexen ordenen voor x-as plot
tidydataIndexP$Index <- factor(tidydataIndexP$Index,levels = c("KS","ETI","RTI","PPVT"))

# Plot met losse datapunten, jitter op positie X en Y
plotIndexP1 <- ggplot(tidydataIndexP, aes(x= Index, y = Score, color = Index, fill = Index)) +
  geom_point(alpha = 0.5, size = 2.3, position = position_jitter(width = 0.07, height = 0.01), na.rm= TRUE) +
  geom_point(data = descriptivesIndexP, size = 3.3, shape = 21, color = "black", alpha = 0.9) +
  geom_errorbar(data = descriptivesIndexP, aes(ymin= lower, ymax = upper), width = 0.2) +
  theme_bw(base_size = 14) +
  # optional: lijnen om SD CELF normen aan te geven
  geom_hline(yintercept = 78, color = "black", alpha = 0.5, size = 1, linetype = "dotdash") +
  geom_hline(yintercept = 85, color = "black", alpha = 0.5, size = 1, linetype = "longdash") +
  geom_hline(yintercept = 115, color = "black", alpha = 0.5,  size = 1, linetype = "longdash") +
  geom_hline(yintercept = 100, color = "black", alpha = 0.5,  size = 1, linetype = "solid")

#geom_errorbar(data = descriptivesIndexP, aes(ymin= lower, ymax = upper), width = 0.2) +

# Plot met losse datapunten, jitter op positie X en Y
ggplot(tidydataIndexP, aes(x= Index, y = Score)) +
  geom_boxplot(size = 0.7, outlier.shape = 21, outlier.size = 3, outlier.alpha = 0.5) + 
  geom_jitter(aes(x= Index, y = Score, color = Index, fill = Index), alpha = 0.5, size = 3, shape = 21, width = 0.2, na.rm= TRUE) +
  theme_bw(base_size = 16) +
  # scale_shape(solid = FALSE) +
  # optional: lijnen om SD CELF normen aan te geven
  geom_hline(yintercept = 78, color = "black", alpha = 0.7, size = 1, linetype = "dotdash") +
  geom_hline(yintercept = 85, color = "black", alpha = 0.7, size = 1, linetype = "longdash") +
  geom_hline(yintercept = 115, color = "black", alpha = 0.7,  size = 1, linetype = "longdash") +
  geom_hline(yintercept = 100, color = "black", alpha = 0.7,  size = 1, linetype = "solid")




# Plot met losse datapunten, jitter op positie X en Y
ggplot(tidydataIndexP, aes(x = Index, y = Score, color = Index, fill = Index)) +
  geom_dotplot(binaxis = "y" , stackdir = "center", dotsize = 0.7, color = "black", alpha = 0.9) +
  theme_bw(base_size = 16) +
  # scale_shape(solid = FALSE) +
  # optional: lijnen om SD CELF normen aan te geven
  geom_hline(yintercept = 78, color = "black", alpha = 0.7, size = 1, linetype = "dotdash") +
  geom_hline(yintercept = 85, color = "black", alpha = 0.7, size = 1, linetype = "longdash") +
  geom_hline(yintercept = 115, color = "black", alpha = 0.7,  size = 1, linetype = "longdash") +
  geom_hline(yintercept = 100, color = "black", alpha = 0.7,  size = 1, linetype = "solid")


boxplot(Score ~ Index, data = tidydataIndexP, 
        outline = FALSE,     ## avoid double-plotting outliers, if any
        main = 'boxplot + beeswarm') +
beeswarm(Score ~ Index, data = tidydataIndexP,
         col = 4, pch = 16, panel.first = abline(3,0), add = TRUE)


# col = 4, pch = 16, add = FALSE

ggplot(data, aes(x= LogoV, y = ETI))+
  geom_point(alpha = 0.3, position = position_jitter(width = 0.15), na.rm= TRUE) +
  geom_smooth (method = "lm", color = "blue", alpha = 0.5, linetype = "solid", size = 0.5, na.rm= TRUE)

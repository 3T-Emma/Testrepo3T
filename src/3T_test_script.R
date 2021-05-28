
##########################################################################################################
                 ### Script Language profile preschoolers with 22q11DS - Everaert/Selten  ### 
##########################################################################################################

# Clear workspace:
rm(list=ls())

## load packages
library(readxl)
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
library(effectsize)

## set working directory
setwd("C:/Users/Emma/Documents/Thuis werken/good-enough-project-template/data/processed")

## load data
data <- read_excel("Datafile_repo.xlsx") #this data has been generated at random 

### inverting LogoV scores 0-4 to 5-1 (LogoV 0 = new 5, 1 = 4, 2 = 3, 3 = 2, 4 = 1)
data$LogoV <- 5-data$LogoV

##########################################################################################################
                                        ### Dataframes ###
##########################################################################################################

# Seperate dataframes for indeces, subtests an/or LogoV
dataIndexP <- data[c("Subject", "PPVT", "KS", "RTI", "ETI")] 
dataERTI <- data[c("Subject", "RTI", "ETI")]
dataSubtest <- data[c("Subject", "ZB", "AW", "ZH", "WS", "BAV", "EC", "WC.R")]
dataIndexLogoV <- data[c("Subject", "PPVT", "KS", "RTI", "ETI", "LogoV")]
IndexPdata <- data[c("Subject","AgeMd", "Sex", "SES", "LogoV", "KS", "ETI", "RTI", "PPVT")]
dataTask_Missing <- data[c("Subject", "PPVT", "ZB", "AW", "ZH", "WS", "BAV", "EC", "WC.R")]

# Long format
tidydataSubtest <- dataSubtest %>% gather(Task, Score, -c(Subject))
tidydataIndexP <- dataIndexP %>% gather(Index, Score, -c(Subject))
tidydataIndexLogoV <- dataIndexLogoV %>% gather(Index, Score, -c(Subject, LogoV))
tidydataERTI <- dataIndexP %>% gather(Index, Score, -c(Subject))

# Complete data
IndexPdata_comp <- IndexPdata[complete.cases(IndexPdata), ]
dataERTI_comp <- dataERTI[complete.cases(dataERTI), ]
ExcludedIndexP <- subset(IndexPdata, is.na(ETI) == TRUE | is.na(RTI) == TRUE | is.na(KS) == TRUE | is.na(PPVT) == TRUE)

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
dataTask_Missing$NMissing = (count_row_if(dataTask_Missing, criterion = NA) -1)  #-1 because they always miss either EC or WC.R because of age
table(dataTask_Missing$NMissing) #table of number of children missing 0, 1, 2, 3, 4, 5, 6 or 7 tasks

#### Check whether participants with missing data differ from those with complete date on age, sex, intelligibility, and parental education

# Age difference included / excluded
wilcox.test(ExcludedIndexP$AgeMd, IndexPdata_comp$AgeMd, conf.int = TRUE)
t.test(ExcludedIndexP$AgeMd, IndexPdata_comp$AgeMd)
mean(ExcludedIndexP$AgeMd)
mean(IndexPdata_comp$AgeMd)
cohens_d(ExcludedIndexP$AgeMd, IndexPdata_comp$AgeMd, var.equal = TRUE, hedges.correction = TRUE)

# Parental education difference included / excluded
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
                                ### Normality and statistics RTI-ETI ###
##########################################################################################################

## outliers
tidydataIndexP %>%
  group_by(Index) %>%
  identify_outliers(Score)
## For the CELF, we have no reason to assume the score of subject X is not representative or an overestimation of their capabilities

## normality qqplot
ggqqplot(data$KS)
ggqqplot(data$RTI)
ggqqplot(data$ETI)
ggqqplot(data$PPVT)
hist(data$KS)
hist(data$RTI)
hist(data$ETI)
hist(data$PPVT)

## Shapiro-Wilk normality
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

# Descripives LogoV
resLogoV <- stat.desc(data$LogoV)
round(resLogoV, 2)
table(data$LogoV)
count_if(gt(2.5), data$LogoV)

### Correlations
cor_test(data, LogoV, AgeMd, method = "pearson", alternative = "two.sided", conf.level = 0.95)
cor_test(data, LogoV, AgeMd, method = "spearman", alternative = "two.sided", conf.level = 0.95)
cor_test(data, LogoV, AgeMd, method = "kendall", alternative = "two.sided", conf.level = 0.95)

# Pearson
cor_mat(data, RTI, ETI, KS, PPVT, LogoV, method = "pearson", alternative = "two.sided", conf.level = 0.95)
cor_test(data, vars = LogoV, vars2 = c(RTI, ETI, KS, PPVT), method = "pearson", alternative = "two.sided", conf.level = 0.95)
# Spearman
cor_mat(data, RTI, ETI, KS, PPVT, LogoV, method = "spearman", alternative = "two.sided", conf.level = 0.95)
cor_test(data, vars = LogoV, vars2 = c(RTI, ETI, KS, PPVT), method = "spearman", alternative = "two.sided", conf.level = 0.95)
# Kendall
cor_mat(data, RTI, ETI, KS, PPVT, LogoV, method = "kendall", alternative = "two.sided", conf.level = 0.95)
cor_test(data, vars = LogoV, vars2 = c(RTI, ETI, KS, PPVT), method = "kendall", alternative = "two.sided", conf.level = 0.95)

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


##########################################################################################################
                ### Categories Results and Figure 2 ###
##########################################################################################################

library(docstring) 
#purely for course on 28-5-2021
#' @title Category: no Problems
#' @description This function counts the number 
#' of participants with scores in the average range
#' criteria of the average range can be specified
#' @param x Language index score
#' @param y Speech intelligibility score 
#' @return The number of participant that meet these criteria

NoProblems <- function(d, x, y){ 
  NP <- count(d %>% filter(x >= cutoff_Index & y >= cutoff_LogoV))
return(NP)
} 

SpeechProblems <- function(d, x, y){ 
  SP <- count(d %>% filter(x >= cutoff_Index & y < cutoff_LogoV))
  return(SP)
} 

LanguageProblems <- function(d, x, y){ 
  LP <- count(d %>% filter(x < cutoff_Index & y >= cutoff_LogoV))
  return(LP)
} 

SpeechLanguageProblems <- function(d, x, y){ 
  SLP <- count(d %>% filter(x < cutoff_Index & y < cutoff_LogoV))
  return(SLP)
} 

#cut-off criteria
cutoff_Index = 85
cutoff_LogoV = 3

#RTI
NoProblems(data, data$RTI, data$LogoV)
SpeechProblems(data, data$RTI, data$LogoV)
LanguageProblems(data, data$RTI, data$LogoV)
SpeechLanguageProblems(data, data$RTI, data$LogoV)

#ETI
NoProblems(data, data$ETI, data$LogoV)
SpeechProblems(data, data$ETI, data$LogoV)
LanguageProblems(data, data$ETI, data$LogoV)
SpeechLanguageProblems(data, data$ETI, data$LogoV)

#KS
NoProblems(data, data$KS, data$LogoV)
SpeechProblems(data, data$KS, data$LogoV)
SpeechLanguageProblems(data, data$KS, data$LogoV)
LanguageProblems(data, data$KS, data$LogoV)

#PPVT
NoProblems(data, data$PPVT, data$LogoV)
SpeechProblems(data, data$PPVT, data$LogoV)
LanguageProblems(data, data$PPVT, data$LogoV)
SpeechLanguageProblems(data, data$PPVT, data$LogoV)


### Figuur Index en PPVT scores ###
# complete data set 
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
ggplot(tidydataIndexP, aes(x= Index, y = Score, color = Index, fill = Index)) +
  geom_point(alpha = 0.5, size = 2.3, position = position_jitter(width = 0.07, height = 0.01), na.rm= TRUE) +
  geom_point(data = descriptivesIndexP, size = 3.3, shape = 21, color = "black", alpha = 0.9) +
  geom_errorbar(data = descriptivesIndexP, aes(ymin= lower, ymax = upper), width = 0.2) +
  theme_bw(base_size = 14) +
  # optional: lijnen om SD CELF normen aan te geven
  geom_hline(yintercept = 78, color = "black", alpha = 0.5, size = 1, linetype = "dotdash") +
  geom_hline(yintercept = 85, color = "black", alpha = 0.5, size = 1, linetype = "longdash") +
  geom_hline(yintercept = 115, color = "black", alpha = 0.5,  size = 1, linetype = "longdash") +
  geom_hline(yintercept = 100, color = "black", alpha = 0.5,  size = 1, linetype = "solid")


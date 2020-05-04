##Vegetaion structure analysis

#library
## Load relevant R packages
library(Rmisc)
library(ggplot2)
library(lme4)
library(car)
library(lmerTest)
library(dplyr)
library(tidyverse)
library(broom)

#=====import data======
#relative pathname
plant <- file.path(".", "Data", "Vegetation Structure Data Main Experiment.csv")
print(plant)

#import data
plt <- read_csv(plant)%>%
  mutate(Block = as.character(Block),
         Plot = as.character(Plot),
         Date = as.character(Date))

glimpse(plt)

#===================================
##Analysis for LAI
#===================================

#LMM test
mod1<-lmer(sqrt(LAI) ~ Midges * Structure * Date + (1|Block/Plot), data = plt)
anova(mod1, ddf = "Kenward-Roger")

##Tukey posthoc comparisons
emmeans(mod1, list(pairwise ~ Structure:Date), adjust = "tukey")


#================================
#analysis for vegetation height
#================================

##LMM
mod2<-lmer(sqrt(Height) ~ Midges * Structure * Date + (1|Block/Plot), data = plt)
anova(mod2, ddf = "Kenward-Roger")

#Tukey's posthoc tests
emmeans(mod2, list(pairwise ~ Structure:Date), adjust = "tukey")


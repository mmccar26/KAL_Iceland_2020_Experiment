##Preliminary analyses on vegetation effects on midge deposition

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
midge <- file.path(".", "Data", "Midge Deposition Data Experiment 2.csv")
print(midge)

#=========================================
#Analysis on infall data
#========================================

#import data on infall deposition
mid<- read_csv(midge)%>%
  mutate(Extraction.Date = as.Date(Extraction.Date),
         Set.Date = as.Date(Set.Date),   
         Day.diff = Extraction.Date - Set.Date)%>%
  filter(Trap == "Infall")%>%
  mutate(Day.diff = as.numeric(Day.diff),
         Midge.day =  Total / Day.diff,
         Extraction.Date = as.character(Extraction.Date))

glimpse(mid)

##to examine full model
mod1<-lmer(log(Midge.day) ~ Treatment * Extraction.Date + (1|Plot), data = mid)
anova(mod1, ddf= "Kenward-Roger")

##To plot model diagnostics
#set plot matrix
par(mfrow=c(1,2)) ## set the plot matrix

##qq norm plots
qqPlot(resid(mod1), xlab="Theoretical Quantiles", ylab = "Sample Quantiles", line = "quartiles", col.lines = "black", grid = FALSE)

##residual vs fitted plot
plot(fitted(mod1), resid(mod1), xlab = "Fitted Residuals", ylab = "Residuals") #residuals vs fitted
abline(h=0)

##==========================
#Analysis on pitfall data
#===========================
#import data
pit<- read_csv(midge)%>%
  mutate(Extraction.Date = as.Date(Extraction.Date),
         Set.Date = as.Date(Set.Date),   
         Day.diff = Extraction.Date - Set.Date)%>%
  filter(Trap == "Pitfall")%>%
  mutate(Day.diff = as.numeric(Day.diff),
         Midge.day =  Total / Day.diff,
         Extraction.Date = as.character(Extraction.Date))

glimpse(pit)

##to examine full model
mod2<-lmer(log(Midge.day) ~ Treatment * Extraction.Date + (1|Plot), data = pit)
anova(mod2, ddf = "Kenward-Roger")

#To plot model dignostics
#set plot matrix
par(mfrow=c(1,2)) ## set the plot matrix

##qq norm plots
qqPlot(resid(mod2), xlab="Theoretical Quantiles", ylab = "Sample Quantiles", line = "quartiles", col.lines = "black", grid = FALSE)

##residual vs fitted plot
plot(fitted(mod2), resid(mod2), xlab = "Fitted Residuals", ylab = "Residuals") #residuals vs fitted
abline(h=0)

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
midge <- file.path(".", "Data", "Canopy Interception Data Experiment 2.csv")
print(midge)

#import data
mid<- read_csv(midge)
glimpse(mid)

##to examine full model
mod1<-lmer(asin(P.intercepted) ~ Treatment * Date + (1|Plot), data = mid)
anova(mod1, ddf = "Kenward-Roger")

##To examine model diagnostics
#set plot matrix
par(mfrow=c(1,2)) ## set the plot matrix

##qq norm plots
qqPlot(resid(mod1), xlab="Theoretical Quantiles", ylab = "Sample Quantiles", line = "quartiles", col.lines = "black", grid = FALSE)

##residual vs fitted plot
plot(fitted(mod1), resid(mod1), xlab = "Fitted Residuals", ylab = "Residuals") #residuals vs fitted
abline(h=0)


##Plot to show data
ggplot(mid, aes(x = Treatment, y = P.intercepted, fill = Treatment))+
  geom_boxplot(size = 1, outlier.shape = NA)+ 
  xlab(NULL) +
  ylab(NULL) +
  scale_y_continuous(limits = c(0.7, 1))+
  scale_fill_manual(name= "Treatment", values=c("gray", "white"))+
  theme(axis.text.x=element_blank(),
        axis.text.y = element_text(colour= "black", size = 12, face = "bold"),
        axis.line = element_line(colour = "black", size = 1),
        axis.line.y = element_line(colour = "black", size =1),
        axis.ticks.x = element_line(colour = "black", size = 1),
        axis.ticks.y = element_line(colour = "black", size = 1),
        axis.ticks.length = unit(.2, "cm"),
        panel.background = element_rect(fill= "white"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#summary statistics
sum.mid<-summarySE(mid, groupvars = "Treatment", measurevar = "P.intercepted")
sum.mid


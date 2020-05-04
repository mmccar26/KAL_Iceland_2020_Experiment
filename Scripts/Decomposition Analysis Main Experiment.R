##Decomposition analysis

#library
## Load relevant R packages
library(ggplot2)
library(Rmisc)
library(lme4)
library(car)
library(emmeans)
library(lmerTest)
library(dplyr)
library(tidyverse)
library(broom)

#=====import data======
#relative pathname
decomp <- file.path(".", "Data", "Decomposition Data Main Experiment.csv")
print(decomp)

#Transfer into appropriate factors and to analyze the first litter bags
#import data
lit <- read_csv(decomp)%>%
  mutate(Block = as.character(Block),
         Time = as.character(Time))%>%
  filter(Time == 2)

##remove outliers for portion decomposed (Ploss > 3 SD)
isnt_out_mad <- function(x, thres = 3, na.rm = TRUE) {
  abs(x - median(x, na.rm = na.rm)) <= thres * mad(x, na.rm = na.rm)
}

lit1 <- 
  lit%>%
  mutate(Outlier = isnt_out_mad(lit$Ploss))%>%
  filter(Outlier == "TRUE")


##to examine full model without outliers
mod1<-lmer(asin(Ploss) ~ Midges * Structure + (1|Block), data = lit1)
anova(mod1, ddf = "Kenward-Roger")

##To evaluate model diagnostics
#set plot matrix
par(mfrow=c(1,2)) ## set the plot matrix

##qq norm plots
qqPlot(resid(mod1), xlab="Theoretical Quantiles", ylab = "Sample Quantiles", line = "quartiles", col.lines = "black", grid = FALSE)

##residual vs fitted plot
plot(fitted(mod1), resid(mod1), xlab = "Fitted Residuals", ylab = "Residuals") #residuals vs fitted
abline(h=0)

##to plot data
lit1<-summarySE(lit, measurevar = "Ploss", groupvars=c("Treatment"))
lit1

#to order treatments
lit1$Treatment <- factor(lit1$Treatment,levels = c("I", "MI", "IT", "C"))

##plot
ggplot(lit1, aes(x = Treatment, y = Ploss, fill = Treatment))+
         geom_bar(position=position_dodge(), stat="identity", color = "black", width=.6)+
         geom_errorbar(aes(ymin=Ploss-se, ymax=Ploss+se), size=1.3, width=0,position=position_dodge(.9))+
         xlab(NULL) +
         ylab(NULL) +
         coord_cartesian(ylim=c(0,1))+
         scale_y_continuous(expand = c(0, 0))+
         scale_fill_manual(values=c("gray", "white", "gray", "white"))+
         theme(axis.text.x = element_blank(),
               axis.text.y = element_text(colour= "black", face = "bold", size = 13),
               axis.line = element_line(colour = "black", size = .3),
               axis.line.x = element_line(colour = "black", size =.3),
               axis.ticks.x = element_line(colour = "black", size = 1),
               axis.ticks.y = element_line(colour = "black", size = 1),
               axis.line.y = element_line(colour = "black", size =.3),
               panel.background = element_rect(fill= "white"),
               legend.position = "none",
               panel.border = element_rect(fill = NA, colour = "black", size = 1.5))
       
##=====Last litter bag analysis======

#filter data for last litter bags
lit2 <- read_csv(decomp)%>%
  mutate(Block = as.character(Block),
                Time = as.character(Time))%>%
         filter(Time == 3)

##to examine full model
mod1<-lmer(asin(Ploss) ~ Midges * Structure + (1|Block), data = lit2)
anova(mod1, ddf = "Kenward-Roger")

#Plot model diagnostics
#set plot matrix
par(mfrow=c(1,2)) ## set the plot matrix

##qq norm plots
qqPlot(resid(mod1), xlab="Theoretical Quantiles", ylab = "Sample Quantiles", line = "quartiles", col.lines = "black", grid = FALSE)

##residual vs fitted plot
plot(fitted(mod1), resid(mod1), xlab = "Fitted Residuals", ylab = "Residuals") #residuals vs fitted
abline(h=0)

##To plot data
lit3<-summarySE(lit2, measurevar = "Ploss", groupvars=c("Treatment"))

#To reorder data
lit3$Treatment<- factor(lit3$Treatment,levels = c("I", "MI", "IT", "C"))
       
#Plot data
ggplot(lit3, aes(x = Treatment, y = Ploss, fill = Treatment))+
   geom_bar(position=position_dodge(), stat="identity", color = "black",  width=.6)+
   geom_errorbar(aes(ymin=Ploss-se, ymax=Ploss+se), size=1.3, width=0,position=position_dodge(.9))+
   xlab(NULL) +
   ylab(NULL) +
   coord_cartesian(ylim=c(0,1))+
   scale_y_continuous(expand = c(0, 0))+
   scale_fill_manual(values=c("gray", "white", "gray", "white"))+
   theme(axis.text.x = element_blank(),
       axis.text.y = element_text(colour= "black", face = "bold", size = 13),
       axis.line = element_line(colour = "black", size = .3),
       axis.line.x = element_line(colour = "black", size =.3),
       axis.ticks.x = element_line(colour = "black", size = 1),
       axis.ticks.y = element_line(colour = "black", size = 1),
       axis.line.y = element_line(colour = "black", size =.3),
       panel.background = element_rect(fill= "white"),
      legend.position = "none",
      panel.border = element_rect(fill = NA, colour = "black", size = 1.5))

##Analysis with outliers included
#setup data
lit <- read_csv(decomp)%>%
  mutate(Block = as.character(Block),
         Time = as.character(Time))%>%
  filter(Time == 2)

##to examine full model
mod1<-lmer(Ploss ~ Midges * Structure + (1|Block), data = lit)
anova(mod1, ddf = "Kenward-Roger")

       



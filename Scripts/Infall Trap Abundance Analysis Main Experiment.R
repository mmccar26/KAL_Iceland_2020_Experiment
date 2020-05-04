##Analysis on Infall Trap Data in main experiment

#library
## Load relevant R packages
library(Rmisc)
library(emmeans)
library(ggplot2)
library(lme4)
library(car)
library(lmerTest)
library(dplyr)
library(tidyverse)
library(broom)

#=====import data======
#relative pathname
midge <- file.path(".", "Data", "Infall Trap Abundance Data Main Experiment.csv")
print(midge)

#import data
mid<- read_csv(midge)%>%
  mutate(Block = as.character(Block),
         Plot = as.character(Plot),
         Extraction.Date = as.Date(Extraction.Date),
         Set.Date = as.Date(Set.Date),
         Day.diff = Extraction.Date - Set.Date)%>%
  mutate(Day.diff = as.numeric(Day.diff),
         Midge.day =  Total / Day.diff,
         Extraction.Date = as.character(Extraction.Date))


##to examine full model
mod1<-lmer(log(Midge.day+1) ~ Midges * Structure * Extraction.Date + (1|Block/Plot), data = mid)
anova(mod1, ddf = "Kenward-Roger")

#Tukey post-hoc test
emmeans(mod1, list(pairwise ~ Midges:Extraction.Date), adjust = "tukey")

##To check model diagnostics
#set plot matrix
par(mfrow=c(1,2)) ## set the plot matrix

##qq norm plots
qqPlot(resid(mod1), xlab="Theoretical Quantiles", ylab = "Sample Quantiles", line = "quartiles", col.lines = "black", grid = FALSE)

##residual vs fitted plot
plot(fitted(mod1), resid(mod1), xlab = "Fitted Residuals", ylab = "Residuals") #residuals vs fitted
abline(h=0)

#plots to show midge abundance through time
mid1<-summarySE(mid, measurevar = "Midge.day", groupvars = c("Treatment", "Extraction.Date"))

pd <- position_dodge(0.3)

ggplot(mid1, aes(x=Extraction.Date, y=Midge.day, group = Treatment, shape = Treatment)) + 
  geom_line(aes(linetype=Treatment), position = pd, color = "black", size = 1.3) +
  geom_errorbar(aes(ymin=Midge.day-se, ymax=Midge.day+se), position = pd, size=1, width=0) +
  geom_point(aes(shape=Treatment, fill = Treatment), position = pd, size = 5)+
  xlab(NULL) +
  ylab(NULL) +
  scale_y_continuous(expand = c(0, 0))+
  scale_x_discrete(breaks=c("2017-08-04","2017-08-20", "2018-06-11", "2018-06-24", "2018-07-20", "2018-08-13"))+
  coord_cartesian(ylim=c(0,450))+
  scale_linetype_manual(values=c("solid", "dashed", "dashed", "solid"))+
  scale_shape_manual(values=c(21, 22, 21, 22))+
  scale_fill_manual(values=c("white","black", "black", "white"))+
  theme(axis.text.x=element_text(colour= "black", size = 12),
        axis.text.y = element_text(colour= "black", size = 12),
        axis.ticks.x = element_line(colour = "black", size = 1),
        axis.ticks.y = element_line(colour = "black", size = 1),
        axis.ticks.length = unit(.2, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black", size = 2),
        legend.position = "none",
        axis.line = element_line(colour = "black"),
        plot.title = element_text(face = "bold", size = 14,hjust = 0.5),
        strip.text.x = element_text(face = "bold", size = 12),
        axis.title.x = element_text(colour= "black", size = 12,  face = "bold",
                                    margin = margin(t = 8, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(colour= "black", size = 12, face = "bold",
                                    margin = margin(t = 0, r = 8, b = 0, l = 0))
  )


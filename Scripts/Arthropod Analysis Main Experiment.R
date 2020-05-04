#=======Arthropod analysis==========

#===load libraries======
library(ggplot2)
library(vegan)
library(plyr)
library(lme4)
library(car)
library(lmerTest)
library(dplyr)
library(tidyverse)
library(Rmisc)
library(broom)

#=====import data======
#relative pathname
vacart <- file.path(".", "Data", "Arthropod Data Main Experiment.csv")
print(vacart)

#import data
art <- read_csv(vacart)%>%
  mutate(Year = as.character(Year),
         Block = as.character(Block),
         Time = as.character(Time))%>%
  filter(Time == "2")%>%
  drop_na()

##===============Univariate analysis==========================
##Last data point

##Analysis for total arthropods
mod1<-lmer(sqrt(Total)~ Midges*Structure + (1|Block), data = art)
anova(mod1)

##Set up data for functional group analysis
#Summarize functional group numbers
fun<-
  art%>%
  mutate(Predators = Mesostigmata + Linyphiidae + Staphylinidae + Opiliones,
         Decomposers = Oribatida+Entomobryidae+Isotomidae+Sminthuridae+Hypogastruridae+Onychuridae+Cryptophagidae,
         Herbivores = Aphididae+Auchenorrhyncha+Coccoidea,
         Pros = Prostigmata + Acari)

#==========================
#Predators
#==========================
mod2<-lmer(log(Predators) ~ Midges*Structure+ (1|Block), data = fun)
anova(mod2, ddf = "Kenward-Roger")

##set up for plots
lit<-summarySE(fun, measurevar = "Predators", groupvars=c("Treatment"))
lit

lit$Treatment <- factor(lit$Treatment,levels = c("I", "MI", "IT", "C"))

ggplot(lit, aes(x = Treatment, y = Predators, fill = Treatment))+
  geom_bar(position=position_dodge(), stat="identity", color = "black", width=.6)+
  geom_errorbar(aes(ymin=Predators-se, ymax=Predators+se), size=1.3, width=0,position=position_dodge(.9))+
  xlab(NULL) +
  ylab(NULL) +
  coord_cartesian(ylim=c(0,70))+
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

#====================================
#Herbivores
#====================================
mod3<-lmer(log(Herbivores+1) ~ Midges*Structure + (1|Block), data = fun)
anova(mod3, ddf = "Kenward-Roger")

##To plot data
lit<-summarySE(fun, measurevar = "Herbivores", groupvars=c("Treatment"))
lit

lit$Treatment <- factor(lit$Treatment,levels = c("I", "MI", "IT", "C"))

ggplot(lit, aes(x = Treatment, y = Herbivores, fill = Treatment))+
  geom_bar(position=position_dodge(), stat="identity", color = "black", width=.6)+
  geom_errorbar(aes(ymin=Herbivores-se, ymax=Herbivores+se), size=1.3, width=0,position=position_dodge(.9))+
  xlab(NULL) +
  ylab(NULL) +
  coord_cartesian(ylim=c(0,15))+
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

#=====================================
#Decomposers
#=====================================
mod4<-lmer(log(Decomposers) ~ Midges * Structure + (1|Block), data = fun)
anova(mod4, ddf = "Kenward-Roger")

##To plot data
lit<-summarySE(fun, measurevar = "Decomposers", groupvars=c("Treatment"))
lit

lit$Treatment <- factor(lit$Treatment,levels = c("I", "MI", "IT", "C"))

ggplot(lit, aes(x = Treatment, y = Decomposers, fill = Treatment))+
  geom_bar(position=position_dodge(), stat="identity", color = "black", width=.6)+
  geom_errorbar(aes(ymin=Decomposers-se, ymax=Decomposers+se), size=1.3, width=0,position=position_dodge(.9))+
  xlab(NULL) +
  ylab(NULL) +
  coord_cartesian(ylim=c(0,1250))+
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

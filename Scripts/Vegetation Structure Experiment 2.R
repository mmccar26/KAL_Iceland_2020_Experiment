
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
plant <- file.path(".", "Data", "Vegetation Structure Data Experiment 2.csv")
print(plant)

#import data
plt <- read_csv(plant)
glimpse(plt)

##to examine full model
t.test(sqrt(Height) ~ Treatment, data = plt)
t.test(sqrt(LAI) ~ Treatment, data = plt)

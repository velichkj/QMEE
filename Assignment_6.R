library(tibble)
library(tidyverse)
library(cowplot)
library(ggplot2)
library(devtools)
library(ggpubr)
library(dplyr)
Fish_dat <- read_csv("New_Brunswick_Fall_2020_fish_data.csv")

Fish_dat <- Fish_dat %>% mutate(log10Hg_ug_per_kgww = log10(Hg_ug_per_kgww)) %>% 
  drop_na(log10Hg_ug_per_kgww)

#For my linear model, I'm going to test whether mercury concentration increases with total length
#I'll also be looking at the difference in mercury concentration between species (Smallmouth Bass and Yellow Perch)
#Specifically, I'm asking: does the length of the fish affect mercury concentration differently for different species?
#First I'll look at the diagnostic plots
#I'll be comparing my mercury concentration between untransformed and log transformed to see which one meets the assumptions better
m1 <- lm(Hg_ug_per_kgww~TotalLength_mm, data=Fish_dat)
par(mfrow = c(2, 2))
plot(m1,id.n=4)

m1 <- lm(log10Hg_ug_per_kgww~TotalLength_mm, data=Fish_dat)
par(mfrow = c(2, 2))
plot(m1,id.n=4)

#It seems like the log transformed mercury concentration meets the assumptions better
#From the diagnostic plot it looks like the log transformed Hg has less heteroscedasticity
#Furthermore the residuals look more normal in the log transformed Hg
#Therefore, I'll keep the log transformed data for my models

#Let's look at various models of how length and species affect Hg
lmboth <- lm(log10Hg_ug_per_kgww~TotalLength_mm + Species, data=Fish_dat)
lmint <- lm(log10Hg_ug_per_kgww~TotalLength_mm*Species, data=Fish_dat)
lmlength <- lm(log10Hg_ug_per_kgww~TotalLength_mm, data=Fish_dat)
lmspecies <- lm(log10Hg_ug_per_kgww~Species, data=Fish_dat)

summary(lmboth) 

library(car)
Anova(lmboth) #I used this to see which variable was explaning less of the variation in log10 Hg conc.
#From the Anova it was species explaining less variation

anova(lmboth, lmlength, test="F") #from this comparison, if you take species out of the model, you don't lose a significant amount of explanatory power
anova(lmboth, lmspecies, test="F") #whereas if you remove total length, you remove explanatory power for log10 Hg conc.
#therefore, length seems to explain more variation in mercury concentration than species

library(emmeans)
e1 <- emmeans(lmboth, "Species")
pairs(e1)
plot(e1)
#you can see that the estimated 95% intervals for the species overlap quite a bit which could explain why it isn't a good predictor of Hg

print(summary(lmint))
#from the interaction, it doesn't seem like the interaction between species and length with respect to mercury concentration
## JD: YOu are missing some words above. "Is strong" might be a good choice. I hope you looked and (and tried to interpret) the values. An effects plot can help.
#i.e. the mercury concentration increases with length similarily between species  

## JD: Nice job with the diagnostic comparison
## You should not go ahead and drop things, though. You have plenty of data, and should just stick with your "full" model. There are two issues now. One is interpreting the main effects in a model with interactions. You can do this by choosing sum-to-zero contrasts on fish (to get a measure of the average slope across species) and by centering length (to get an estimate of the difference between species at a sensible point on the length curves). All of this would be clarified by a nice effects plot of the whole model 
## This effects plot should probably also be done on the original scale (not the log scale as your current effects plot) for biological interpretability

## Grade 2.0/3

library(tibble)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)
library(ggpubr)
library(jtools)
Fish_dat <- read_csv("New_Brunswick_Fall_2020_fish_data.csv")

#I'm going to fit my length vs mercury data for SMB to a generalized linear model
Adult_SMB <- Fish_dat %>% 
  filter(Species == "Smallmouth Bass" & AgeCategory == "Adult") %>% 
  drop_na(Hg_ug_per_kgww, TotalLength_mm)

#First I will plot the data to see what the trend in the data looks like 
gg0 <- ggplot(Adult_SMB,aes(TotalLength_mm,Hg_ug_per_kgww)) +
  geom_point()
print(gg0)
#From this plot it kind of looks like it would follow a log link model

#I'll use the Gamma family, which has a log link by default, because my mercury data are positive continous
gg1 <- gg0 + geom_smooth(method="glm",colour="red",
                         formula=y~x,
                         method.args=list(family="Gamma"))
print(gg1)
#The line seems to fit my data fairly well but let's test it statistically

g1 <- glm(Hg_ug_per_kgww~TotalLength_mm,Adult_SMB,family=Gamma(link="log"))
print(g1)
summary(g1)
#The data are overdispersed by a factor of 3.38 which is quite high, however the gamma model accounts for overdispersion so I should not have to worry about this affecting how confident I can be in the fit of my model
#My p value is very small which shows that I can be confident that mercury concentration is increasing with total length 

#However, it's good to look at the diagnostic plots of this model to see if it's violating any assumptions
par(mfrow = c(2, 2))
plot(g1,id.n=4)
#The residuals seem to make a parabolic curve, which means that a non-linear model may be left out by the g1 model and therefore was left out in the residuals
#To deal with this I will try make a polynomial model instead of linear model
#Nevertheless, the residuals seem to be normally distrbuted 
#Also, the residuals seem to be spread equally along the ranges of predictors
#There seem to be four outliers in my data. Maybe they will be less prevelant in the polynomial model.

#Making a quadratic model using the Gamma family
g2 <- update(g1, Hg_ug_per_kgww~poly(TotalLength_mm,2))
gg2 <- gg0 + geom_smooth(method="glm",formula=y~poly(x,2),
             method.args=list(family="Gamma"))
print(gg2)
summary(g2)
#You can see from the estimate of the linear model that the trend is clearly positive but also from the quadratic model that that trend is leveling out
#My dispersion parameter is a bit lower for the quadratic model (0.226 compared to 0.243 for the linear) because I'm explaining a bit more variability with this model

#Let's test the two models to see if the quadratic fits better than the linear
anova(g1,g2,test="F")
#I get a pretty small p value so I can take that as the quadratic model is explaining the variation in my data better than the linear model

#Let's look at the diagnostic plots for the quadratic model
par(mfrow = c(2, 2))
plot(g2,id.n=4)
#There is no longer a clear trend in the residuals
#The residuals seem to be normally distributed
#The residuals seem to be spread fairly equally along the ranges of predictors
#Now I only have 2 outliers

#Let's plot the models against each other 
print(gg2 <- gg1+geom_smooth(method="glm",formula=y~poly(x,2),
                             method.args=list(family="Gamma")))
#The quadratic model seems to have a narrower standard error at the high length values

#Let's plot some inferential plots for both models using the effect_plot function
#Here we can see what the regression model is telling us by looking at what kind of predictions it generates
effect_plot(g2, pred = TotalLength_mm, interval = TRUE,
            int.type = "confidence",  data = Adult_SMB)
#For the quadratic model it's predicting that mercury goes from decreasing as you increase in length for smaller fish to increasing as you increase length for larger fish 

effect_plot(g1, pred = TotalLength_mm, interval = TRUE,
            int.type = "confidence",  data = Adult_SMB)
#For the linear model, mercury just increases with length 

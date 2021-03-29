library(R2jags)
library(readr)
library(dplyr)
library(ggplot2)

#Input data
fish_dat <- read_csv("New_Brunswick_Fall_2020_fish_data.csv")
summary(fish_dat)

#Filter out species and sites interested in 
#I'll be looking at a Bayesian model of mercury concentration vs length and site in Smallmouth Bass from sites upstream and downstream the dam
target <- c("REF", "NF")
fish_dat <- fish_dat %>% 
  filter(Species == "Smallmouth Bass" & AgeCategory == "Adult" & Site %in% target)
head(fish_dat)

#Make a dataframe of the variables I'm interested in 
fish_dat <- mutate(fish_dat, Site = ifelse(Site == "REF",0,1)) #0 = REF (upstream of dam)
head(fish_dat)
fish_list <- with(fish_dat, list(N = nrow(fish_dat), 
                               Site=as.numeric(Site), TotalLength=TotalLength_mm, Hg=Hg_ug_per_kgww))


#Bayesian model
#I'm going to use pretty vague priors with lower precision for my first Bayesian model
#I'm setting it up with a 0 mean and 0.001 precision as I'm using a less informative value for my first model
fish_model <- function(){
  for (i in 1:N){
    Hg[i] ~ dnorm(pred[i], tau)
    pred[i] <- intercept[1] + Site[i] * intercept[2] + (slope[1] + slope[2] * Site[i]) * TotalLength[i]
  }
  # Priors:
  for (i in 1:2){ # Loop over each treatment
    intercept[i] ~ dnorm(0, 0.001) 
    slope[i] ~ dnorm(0, 0.001) 
  }
  tau ~ dgamma(.001, .001)
}

bayes_lm <- jags(data = fish_list,  model.file = fish_model, inits = NULL,
               parameters.to.save = c("intercept", "slope", "tau"),
               n.chains = 3, n.iter = 12000, n.burnin = 2000, n.thin = 10, DIC = F)

#[1] is 0 (REF-upstream) and [2] is 1 (NF-downstream)
print(bayes_lm)
#From this it looks from these credible intervals that there's a 95% chance, given my prior assumptions, that...
#the true slope for Hg vs length lies between the 2.5% and 97.5% estimates 
#Rhat values are showing how well my chain is doing and those values are close to 1 so that's good
#Also the n effective values are all 1000 or above so the autocorrelation should not be concerning

traceplot(bayes_lm)
#From this plot, we can see that the chains are quite nicely mixed

#Comparing to frequentist model 
dat <- read_csv("New_Brunswick_Fall_2020_fish_data.csv")
target <- c("REF", "NF")
dat <- dat %>% 
  filter(Species == "Smallmouth Bass" & AgeCategory == "Adult" & Site %in% target)

freq_lm <- lm(Hg_ug_per_kgww ~ TotalLength_mm + Site, data=dat)
summary(freq_lm)
confint(freq_lm)
#I'm not 100% sure if I did this right 
#but it looks like in this case that, with a large number of repeated samples, 95% of such calculated confidence intervals would include the true value of the parameter mean 
#In this case for SMB downstream of the dam it would lie between -538 and -33 and for upstream it would lie between -433 and -247





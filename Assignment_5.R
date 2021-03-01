library("ggplot2")
library("lmPerm")
library("coin")
library("gtools")
Fish_dat <- read_csv("New_Brunswick_Fall_2020_fish_data.csv")

#log transform data (to gain a normal distribution) and select variables of interest 
Fish_dat <- Fish_dat %>% mutate(log10Hg_ug_per_kgww = log10(Hg_ug_per_kgww)) %>% 
  select(Fish, Species, Site, AgeCategory, log10Hg_ug_per_kgww, Hg_ug_per_kgww, TotalLength_mm) %>% 
  drop_na(log10Hg_ug_per_kgww)

print(Fish_dat)

#Hypothesis 1: There is a difference in mercury concentration between Smallmouth Bass from upstream (REF) vs downstream (NF) of the dam 

#First, let's select the species, age category and sites (because only one fish was caught at the third site)
target <- c("REF", "NF")
Adult_SMB <- Fish_dat %>% 
  filter(Species == "Smallmouth Bass" & AgeCategory == "Adult" & Site %in% target) %>%
  mutate(log10Hg_ug_per_kgww = log10(Hg_ug_per_kgww))

print(Adult_SMB)

ggplot(Adult_SMB,aes(x=Site,y=log10Hg_ug_per_kgww, fill=Site)) + 
  geom_boxplot() +
  labs(y = "Log transformed Hg concentration (ug/kgww)") +
  scale_x_discrete(limits = c("REF", "NF"), labels=c("Upstream", "Downstream")) +
  theme(legend.position = "none")
#From this plot, it looks like the downstream SMB have higher Hg concentrations, but let's see what the permuation and t test says

#Permutation
set.seed(101)
nsim <- 9999
res <- numeric(nsim) 
for (i in 1:nsim) {
  perm <- sample(nrow(Adult_SMB))
  bdat <- transform(Adult_SMB,log10Hg_ug_per_kgww=log10Hg_ug_per_kgww[perm])
  res[i] <- mean(bdat$log10Hg_ug_per_kgww[bdat$Site=="REF"])-
    mean(bdat$log10Hg_ug_per_kgww[bdat$Site=="NF"])
}

obs <- mean(Adult_SMB$log10Hg_ug_per_kgww[Adult_SMB$Site=="REF"])-
  mean(Adult_SMB$log10Hg_ug_per_kgww[Adult_SMB$Site=="NF"])

res <- c(res,obs)

2*(sum(res>=obs)/(nsim+1))

#Let's plot this 
hist(res,col="gray",las=1,main="")
abline(v=obs,col="red")

par(las=1,bty="l")
plot(prop.table(table(round(res,2))),
     ylab="Proportion",axes=FALSE)
axis(side=2)
axis(side=1,at=seq(-4,4,by=2))
points(obs,0,pch=16,cex=1.5,col="red")

#You can see that my observed value falls far to the left of the distribution generated from the permutation
#From the permutation test and visualizations of the permutation test, I conclude that the difference in the means observed (Hg concentration between the site upstream and downstream of the dam) is due to something real about the system
#However, I should not conclude that Hg concentrations in fish downstream of the dam are higher than upstream because I only looked at 1 site in each location

#Let's see what the t test says 
(tt <- t.test(log10Hg_ug_per_kgww~Site,data=Adult_SMB,var.equal=TRUE))

#The t test had a significant p value (<0.05) which supports the idea that the difference in the means observed is due to something other than chance

#Hypothesis 2: Hg increases with fish length

ggplot(Fish_dat, aes(TotalLength_mm, log10Hg_ug_per_kgww)) + 
  geom_point() +
  geom_smooth(method="lm", formula=y~x)
#From this plot it looks like Hg does increase with fish length but let's check using the permutations and a linear model test 

#Permutation
simfun_rsamp2 <- function(respvar="log10Hg_ug_per_kgww",data=Fish_dat) {
  permdat <- data
  permdat[[respvar]] <- sample(permdat[[respvar]])
  permdat
}

sumfun_Hg <- function(dat) {
  coef(lm(log10Hg_ug_per_kgww~TotalLength_mm,data=dat))["TotalLength_mm"]
}

set.seed(101)
permdist_Hg <- replicate(8000,sumfun_Hg(simfun_rsamp2()))
(Hg_pval <- mean(abs(permdist_Hg)>=abs(sumfun_Hg(Fish_dat))))

#I got a p value of 0 from my permutation test which means that it must be a very small p value
#This supports the idea that Hg is actually increasing in with fish length in this system

#Linear model
summary(lm(log10Hg_ug_per_kgww~TotalLength_mm,data=Fish_dat))

#The p value linear model supports the idea above as a p value <0.05 says that this regression observed would unklikely occur just by chance

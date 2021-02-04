library(tidyverse)
library(cowplot)
library(skimr)
dat <- read_csv("New_Brunswick_Fall_2020_fish_data.csv")
summary(dat)
skim(dat)
#JV: The max TotalWeight_g seems high but we did catch some really big Smallmouth bass - the range just seems odd (huge) because both YOY and adults were caught
#JV: All the variables are under the right category (numeric, character and date)

#JV: To have the data long, each fish ID (Fish) should have a separate row (which is what they have in the csv file right now). However if there are duplicates this could affect analyses. 
print(dat
      %>% group_by(Fish)
      %>% summarize(count = n())
      %>% filter(count>1)
)
#JV: From this tibble I can see there are no duplicates in Fish.

#JV: Visualize the fish Total Length 
ggplot(dat,aes(x=TotalLength_mm)) + geom_density(aes(fill=Species),colour=NA,alpha=0.2) +
  geom_rug(aes(colour=Species)) + scale_x_log10()
#JV: There is a bimodal distribution for the total length for each species because we caught two age categories (YOY and adult)

#JV: Two variables that are often plotted against each other are total length and mercury concentration so let's check their distribution to see if it's normal
#JV: Since I've only measured Hg concentration in the adults so far, I will separate those from YOY
#JV: I will also separate species into their own data frame for graphing/analyses

Adult_SMB <- dat %>% 
  filter(Species == "Smallmouth Bass" & AgeCategory == "Adult")

Adult_YP <- dat %>% 
  filter(Species == "Yellow Perch" & AgeCategory == "Adult")

g1 <- ggplot(Adult_SMB, aes(x = TotalLength_mm)) +
  geom_histogram(na.rm = TRUE)
print(g1)

g2 <- ggplot(Adult_SMB, aes(x = Hg_ug_per_kgww)) +
  geom_histogram(na.rm = TRUE)
print(g2)

g3 <- ggplot(Adult_YP, aes(x = TotalLength_mm)) +
  geom_histogram(na.rm = TRUE)
print(g3)  

g4 <- ggplot(Adult_YP, aes(x = Hg_ug_per_kgww)) +
  geom_histogram(na.rm = TRUE)
print(g4)

#JV: The distributions look a little off, but let's let's test if they're normal using the Shapiro Wilk test

shapiro.test(Adult_SMB$TotalLength_mm)
shapiro.test(Adult_SMB$Hg_ug_per_kgww)

shapiro.test(Adult_YP$TotalLength_mm)
shapiro.test(Adult_YP$Hg_ug_per_kgww)

#JV: The null hypothesis for Shapiro Wilk test is that the data are normal. All of my p-values reject the null and so my data are not normally distributed. 
#JV: Let's make a log transformation to Hg_ug_per_kgww as this is typically plotted on the y axis. 

Adult_SMB <- Adult_SMB %>% mutate(
  log10Hg_ug_per_kgww = log10(Hg_ug_per_kgww))

Adult_YP <- Adult_YP %>% mutate(
  log10Hg_ug_per_kgww = log10(Hg_ug_per_kgww))

#JV: Let's check the distribution again 

g5 <- ggplot(Adult_SMB, aes(x = log10Hg_ug_per_kgww)) +
  geom_histogram(na.rm = TRUE)
print(g5)

g6 <- ggplot(Adult_YP, aes(x = log10Hg_ug_per_kgww)) +
  geom_histogram(na.rm = TRUE)
print(g6)

shapiro.test(Adult_SMB$log10Hg_ug_per_kgww)
shapiro.test(Adult_YP$log10Hg_ug_per_kgww)

#JV: The distribution of the YP Hg concentration passes the Shapiro Wilk test now that it's log transformed, however, the SMB Hg concentration still does not
#JV: I am unsure of ways to tell what transformations would be appropriate for my data (most Hg concentration data is log10 transformed). Maybe this is something that will be looked at in class in the future?

#JV: Let's visualize the data one more time to see the range of the Hg concentrations for both species and for each site in both species
#JV: I will plot a horizontal red line that indicates the Canadian guideline for Hg concentration in retail fish (500ug/kgww)
#JV: Sites will be ordered from upstream of the dam (REF = reference) to downtream of the dam (NF = near field and then FF = far field)

ggplot(dat, aes(x=Species, y=Hg_ug_per_kgww), abline(h=500, col="Red")) + 
  geom_dotplot(binaxis='y', stackdir='center', binwidth = 35, na.rm = TRUE) +
  geom_abline(intercept = 500, slope = 0, color="red") +
  theme_cowplot(12) +
  labs(x="Species", y="Mercury concentration (ug/kgww)")

#JV: Note, site FF is removed from this graph because only 1 SMB was caught there
ggplot(Adult_SMB, aes(x=Site, y=Hg_ug_per_kgww), abline(h=500, col="Red")) + 
  geom_dotplot(binaxis='y', stackdir='center', na.rm = TRUE) +
  geom_abline(intercept = 500, slope = 0, color="red") +
  theme_cowplot(12) +
  labs(x="Site", y="Mercury concentration (ug/kgww)") +
  ggtitle("Mercury in Smallmouth Bass") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(limits = c("REF", "NF"))

ggplot(Adult_YP, aes(x=Site, y=Hg_ug_per_kgww), abline(h=500, col="Red")) + 
  geom_dotplot(binaxis='y', stackdir='center', na.rm = TRUE) +
  geom_abline(intercept = 500, slope = 0, color="red") +
  theme_cowplot(12) +
  labs(x="Site", y="Mercury concentration (ug/kgww)") +
  ggtitle("Mercury in Yellow Perch") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_discrete(limits = c("REF", "NF", "FF"))


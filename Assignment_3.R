library(tidyverse)
library(cowplot)
library(ggplot2)
dat <- read_csv("New_Brunswick_Fall_2020_fish_data.csv")

#From my last assignment, I decided to log transform my Hg concentration because it led to a more normal distribution
## BMB: normality may not be that important, but log-transformation
##  can put the data on a more useful/interpretable scale.
## However, it can also be good to let ggplot do the logging for you
## (via e.g. scale_y_log10())

#I also separated Adults from YOY and Smallmouth Bass from Yellow Perch into different data frames for graphing and analyses
#I'll also mutate the full data frame in case I want to use SMB and YP in the same graph (there is no YOY mercury data so no need to filter out)
dat <- dat %>% mutate(log10Hg_ug_per_kgww = log10(Hg_ug_per_kgww))

Adult_SMB <- dat %>% 
  filter(Species == "Smallmouth Bass" & AgeCategory == "Adult") %>% 
  mutate(log10Hg_ug_per_kgww = log10(Hg_ug_per_kgww))

Adult_YP <- dat %>% 
  filter(Species == "Yellow Perch" & AgeCategory == "Adult") %>% 
  mutate(log10Hg_ug_per_kgww = log10(Hg_ug_per_kgww))

#I'm going to make violin plots to show the distribution of Hg concentration at each site (separate graphs for each species)
#I'm using a violin plot instead of simply as boxplot to have a better visualization of the distribution of the data
#However, I will superimpose and boxplot so that I can still see the descriptive stats (median, IQR, outliers)
#I will also order the sites from upstream of the dam (REF) to downstream (NF, FF)

## BMB: it's usually a mistake to split data and repeat code for each
## component - can use facet_wrap() to do this more easily/automatically

#SMB Hg concentration (remember, FF excluded because only 2 fish caught there)

## BMB: I'm not sure na.rm=TRUE is relevant to ggplot functions
## you probably have to Adult_SMB %>% drop_na(log10Hg_ug_per_kgww) or something
ggplot(Adult_SMB, aes(x= Site, y = log10Hg_ug_per_kgww, fill = Site, na.rm = TRUE)) +
  geom_violin(trim=FALSE, na.rm=TRUE) +
  geom_boxplot(width=0.1, fill = "white", na.rm = TRUE) +
  theme_cowplot(12) +
  labs(x="Site", y="Log transformed Hg concentration (ug/kgww)") +
  scale_x_discrete(limits = c("REF", "NF")) +
  theme(legend.position="none")

#I got a warning message that 2 rows contain missing values (stat_boxplot), however I used na.rm=TRUE in geom_boxplot
#Maybe it's because I have 2 fish actually caught for FF but I excluded the site because that isn't a large enough population to represent the site

#YP Hg concentration 
ggplot(Adult_YP, aes(x= Site, y = log10Hg_ug_per_kgww, fill = Site, na.rm = TRUE)) +
  geom_violin(trim=FALSE, na.rm=TRUE) +
  geom_boxplot(width=0.1, fill = "white", na.rm = TRUE) +
  theme_cowplot(12) +
  labs(x="Site", y="Log transformed Hg concentration (ug/kgww)") +
  scale_x_discrete(limits = c("REF", "NF", "FF")) +
  theme(legend.position="none")

#Let's also facet these graphs so we can see the species side by side 
ggplot(dat, aes(x= Site, y = log10Hg_ug_per_kgww, fill = Site, na.rm = TRUE)) +
  geom_violin(trim=FALSE, na.rm=TRUE) +
    geom_boxplot(width=0.2, fill = "white", na.rm = TRUE,
                 notch=TRUE) +
  theme_cowplot(12) +
  labs(x="Site", y="Log transformed Hg concentration (ug/kgww)") +
  scale_x_discrete(limits = c("REF", "NF", "FF")) +
  theme(legend.position="none") +
    facet_grid(.~Species)
## BMB: yes, this is what I was suggesting ... adding notch=TRUE
## gives approximate CIs on the median (I widened the boxplots to
## make them a little easier to see)

#It's interesting that for SMB, the Hg concentration was highest just downstream of the dam (NF), however in YP, the Hg concentration was highest upstream of the dam (REF), and lowest in the site just downstream of the dam (NF)
#Perhaps this difference could be from a difference in food sources within each species at each site
#I am going to perform of a stable isotope analysis of carbon (δ¹³C) to determine their reliance on terrestrial or aquatic food sources and a stable isotope analysis of nitrogen (δ¹⁵N) to indicate the trophic level of different organisms
#Hopefully these analyses can help explain the difference in mercury concentrations between sites

## BMB: a fundamental issue with this study (and with almost all large-scale
## environmental studies) is that you with only two sites, and without any
## ability to manipulate or randomize the sites, you can never really be
## sure _what_ is causing differences between the sites ...

#A general trend in mercury in fish is that Hg concentration increases with length
#I will make a plot that plots total length vs Hg concentration and run a line of best fit through the data
#Separate lines will be used for each species and each site within each species
#I chose to use shapes for species and colour for site because the species have different sizes which separates them along the x axis, however the YP points overlap a lot so it's more clear to distinguish the sites with colour
ggplot(dat, aes(x= TotalLength_mm, y = log10Hg_ug_per_kgww, colour=Site, shape=Species, group=interaction(Site, Species), na.rm=TRUE)) +
  geom_point(na.rm=TRUE, size = 2.2) +
  geom_smooth(aes(x = TotalLength_mm, y = log10Hg_ug_per_kgww), method = "lm", na.rm=TRUE, se=FALSE) +
  theme_cowplot(12) +
  labs(x="Total fish length (mm)", y="Log transformed Hg concentration (ug/kgww)", shape="Species", colour="Site")


## BMB: I strongly dislike omitting confidence intervals. You
## can use 'alpha' settings to lighten them
## You don't need to keep repeating aesthetics, they're automatically
## 'inherited' from the main ggplot call
## Grouping works automatically in this case
dat %>% drop_na(log10Hg_ug_per_kgww) %>%
ggplot(aes(x= TotalLength_mm, y = log10Hg_ug_per_kgww,
                colour=Site, shape=Species, linetype=Species)) +
    geom_point(size = 2.2) +
    scale_shape_manual(values=c(1,16)) +
    geom_smooth(formula=y~x, method = "lm", se=TRUE, alpha=0.2) +
    theme_cowplot(12) +
    labs(x="Total fish length (mm)", y="Log transformed Hg concentration (ug/kgww)")

#Its seems that within each site, SMB and YP tend to have a similar trend (similar slope) of an increase in Hg with length 
#So essentially, there is a difference in the relationship between fish length and Hg concentration between the sites (specifically NF and REF), and this trend is common for both species
#It seems that in REF, there is a greater increase in Hg concentration with total fish length compared to NF and FF
#Also, it appears that in REF, YP have a higher Hg concentration per length than SMB, whereas in NF, this trend is the opposite
#This may come down again to differences in food sources and relative trophic position within each site but I never would have expected YP to have a greater Hg concentration per length than SMB because SMB tend to be higher in the food web and Hg biomagnifies
#Although, it's also important to note that the range in fish lengths between the species barely overlap so I am just making an extrapolation that the same trend would occur in YP if it could be measured at longer lengths, but I can't definitively say this would be true so this brings up a limitation when comparing between species
#Nevertheless, these are all assumptions made on the trends I'm seeing in the graph but I would need to do an ANCOVA to say whether these trends are significant or not

#Let's try make this plot using facets for different species so that the data are a little less noisy on the graph
ggplot(dat, aes(x= TotalLength_mm, y = log10Hg_ug_per_kgww, colour=Site, group=interaction(Site, Species), na.rm=TRUE)) +
  geom_point(na.rm=TRUE, size = 2.2) +
  geom_smooth(aes(x = TotalLength_mm, y = log10Hg_ug_per_kgww), method = "lm", na.rm=TRUE, se=F) +
  theme_cowplot(12) +
  labs(x="Total fish length (mm)", y="Log transformed Hg concentration (ug/kgww)", colour="Site") +
  facet_grid(.~Species) 

#Now it looks like YP in REF don't have a greater Hg concentration per unit of length
#This is because I'm comparing lines that are on different locations on the x axis
#Therefore, I don't find this graph helpful when comparing the data between species

#Let's try doing a facet but for site instead of species
## BMB: yes, I think this is clearest if you're trying to do species
## comparisons
ggplot(dat, aes(x= TotalLength_mm, y = log10Hg_ug_per_kgww, colour=Species, group=interaction(Site, Species), na.rm=TRUE)) +
  geom_point(na.rm=TRUE, size = 2.2) +
  geom_smooth(aes(x = TotalLength_mm, y = log10Hg_ug_per_kgww), method = "lm", na.rm=TRUE, se=F) +
  theme_cowplot(12) +
  labs(x="Total fish length (mm)", y="Log transformed Hg concentration (ug/kgww)", colour="Species") +
  facet_grid(.~Site) 

#I guess this graph is a little cleaner to look at but it still takes my brain a while to make comparisons across the data
#Overall I enjoy the first graph I made of length vs Hg concentration with both species and all sites on the same graph because it's easier to make all the different kinds or comparisons (between sites within species, between species within sites) when it's all right on the same scale and graph
#I know that the facet graphs are also on the same scale but it's still easier to compare the slopes and intercepts for me when they're all on the same graph
#I think this agrees with Cleveland's Hierarchy as well

## BMB: yes!
## overall I think this is good.  You can simplify a lot of your code
## using some of the stuff I showed in that one example ...

## grade: 2.3/3

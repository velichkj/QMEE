# BMB: please avoid spaces in file names ...
library(tidyverse)  ## tidyverse includes readr, no need to load it separately

# BMB: PLEASE avoid absolute paths.
dat <- read_csv("New_Brunswick_Fall_2020_fish_data.csv")
summary(dat)

smb <- dat %>% filter(Species == "Smallmouth Bass")
mean_smb_Hg <- mean(smb$Hg_ug_per_kgww, na.rm = TRUE)
print(mean_smb_Hg)

yp <- dat %>% filter(Species == "Yellow Perch")
mean_yp_Hg <- mean(yp$Hg_ug_per_kgww, na.rm = TRUE)
print(mean_yp_Hg)

## BMB, or:

(dat
    %>% group_by(Species)
    %>% summarise_at("Hg_ug_per_kgww", mean, na.rm=TRUE)
)

## score: 1.9

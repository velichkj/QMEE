library(tidyverse)
library(readr)

dat <- read_csv("/Users/jennivelichka/Bio 708/QMEE/New_Brunswick_Fall_2020_fish_data.csv")
summary(dat)

smb <- dat %>% filter(Species == "Smallmouth Bass")
mean_smb_Hg <- mean(smb$Hg_ug_per_kgww, na.rm = TRUE)
print(mean_smb_Hg)

yp <- dat %>% filter(Species == "Yellow Perch")
mean_yp_Hg <- mean(yp$Hg_ug_per_kgww, na.rm = TRUE)
print(mean_yp_Hg)

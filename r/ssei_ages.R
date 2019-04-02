##SSEI LL SURVEY AND FISHERY AGE DATA ANALYSES##
library(tidyverse)
library(readxl)
library(ggthemes)
library(extrafont)

##THEMES FOR GRAPHS##
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_bw(base_size=14,base_family='Times New Roman')
          +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()))

#IMPORT SURVEY AGE DATA VIA OCEANAK AND FISHERY AGE DATA VIA ALEX#
survey_age <- read_excel("H:/Groundfish/Sablefish/Clarence/ssei_sablefish/data/survey/raw_data/SSEI LL survey bio data.xlsx")

fishery_age <- read.csv("H:/Groundfish/Sablefish/Clarence/ssei_sablefish/data/fishery/raw_data/SSEI port sampling data.csv", header = TRUE)


#LL SURVEY AGE FREQUENCIES BY YEAR & SEX#
survey_age$`Age Readability`<-as.character(survey_age$`Age Readability`)

ssei_age <- survey_age %>% filter(Age != "NA",Sex %in% c("Male", "Female"), `Age Readability` %in% c("Very Sure", "Comfortably Sure", "Fairly Sure")) %>% 
  group_by(Year, Sex, Age) %>% count(Age)

tiff(filename = "H:/Groundfish/Sablefish/Clarence/ssei_sablefish/figures/LL Survey Age.tiff",
     width = 8, height = 7, units = "in", res = 600, compression = "lzw")


ggplot(ssei_age, aes(Year, Age, size = n)) + geom_point(shape = 21, stroke = 0.5) + 
  facet_wrap(~Sex, ncol = 1) + ylab("Observed Age") + theme(legend.position = "none") + 
  scale_size(range = c(0, 8)) +
  scale_x_continuous(breaks=seq(1985, 2019, 2)) + 
  scale_y_continuous(breaks = seq(0, 50, 10), limits = c(0, 50))

dev.off()

#SEPARATE FISHERY DATA BY GEAR TYPE#
#SSEI PORT LL GEAR#
ssei_ll<-fishery_age %>% filter(PROJECT_CODE == 2)

#SSEI PORT POT GEAR#
ssei_pot<-fishery_age %>% filter(PROJECT_CODE == 17)

#LL FISHERY AGE FREQUENCIES BY YEAR & SEX#
age_ll<-ssei_ll %>% filter(AGE != "NA", SAMPLE_TYPE == "Random",SEX_CODE %in% c(1, 2), AGE_READABILITY_CODE %in% c(1, 2, 3)) %>% 
  mutate(Sex = 
           ifelse(SEX_CODE %in% 1, "Male",
                  ifelse (SEX_CODE %in% 2, "Female", "Other"))) %>%
  group_by(YEAR, Sex, AGE) %>% count(AGE)

tiff(filename = "H:/Groundfish/Sablefish/Clarence/ssei_sablefish/figures/LL Fishery Age.tiff",
     width = 8, height = 7, units = "in", res = 600, compression = "lzw")

ggplot(age_ll, aes(YEAR, AGE)) + geom_point(aes(size = n), shape = 21, stroke = 0.5) +
  scale_size(range = c(0, 8)) + facet_wrap(~Sex, ncol = 1) + 
  theme(legend.position = "none") + ylab("Observed Age") + xlab("Year") +
  scale_x_continuous(breaks = seq(1985, 2019, 2)) + 
  scale_y_continuous(breaks = seq(0, 50, 10), limits = c(0, 50))

dev.off()

#POT FISHERY AGE FREQUENCIES BY YEAR & SEX#
age_pot<-ssei_pot %>% filter(AGE != "NA", SEX_CODE %in% c(1, 2), AGE_READABILITY_CODE %in% c(1, 2, 3)) %>% 
  mutate(Sex = 
           ifelse(SEX_CODE %in% 1, "Male",
                  ifelse (SEX_CODE %in% 2, "Female", "Other"))) %>%
  group_by(YEAR, Sex, AGE) %>% count(AGE)

tiff(filename = "H:/Groundfish/Sablefish/Clarence/ssei_sablefish/figures/Pot Fishery Age.tiff",
     width = 6, height = 5.2, units = "in", res = 600, compression = "lzw")

ggplot(age_pot, aes(YEAR, AGE)) + geom_point(aes(size = n), shape = 21, stroke = 0.5) +
  scale_size(range = c(0, 8)) + facet_wrap(~Sex, ncol = 1) + 
  theme(legend.position = "none") + ylab("Observed Age") + xlab("Year")  +
  scale_x_continuous(breaks = seq(1985, 2019, 2)) +
  scale_y_continuous(breaks = seq(0, 50, 10), limits = c(0, 50))

dev.off()

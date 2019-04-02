##SSEI LL SURVEY AND FISHERY LENGTH FREQUENCIES#
library(dplyr)
library(ggplot2)
library(readxl)
library(ggthemes)
library(gridExtra)
library(extrafont)
library(ggridges)

##THEMES FOR GRAPHS##
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_bw(base_size=14,base_family='Times New Roman')
          +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()))

#IMPORT SURVEY AGE DATA VIA OCEANAK AND FISHERY LENGTH DATA VIA ALEX#
survey <- read_excel("H:/Groundfish/Sablefish/Clarence/ssei_sablefish/data/survey/raw_data/SSEI LL survey bio data.xlsx")

fishery <- read.csv("H:/Groundfish/Sablefish/Clarence/ssei_sablefish/data/fishery/raw_data/SSEI port sampling data.csv", header = TRUE)


#Lengths by Year for SSEI
#Need to find better code for flipping y-axis levels
fish_length <- fishery %>% filter(G_MANAGEMENT_AREA_CODE == "SSEI", SEX_CODE != 0) %>%
  mutate(length_cm = LENGTH_MILLIMETERS / 10, survey_type = ifelse(PROJECT_CODE == 2, "Longline", "Pot"),
                                                            sex =ifelse(SEX_CODE == 1, "Male", 
                                                            ifelse(SEX_CODE == 2, "Female", "Unknown")))

fish_length$YEAR <- as.factor(fish_length$YEAR)

tiff(filename = "H:/Groundfish/Sablefish/Clarence/ssei_sablefish/figures/ssei_fishery_lengths.tiff",
     width = 8, height = 9, units = "in", res = 600, compression = "lzw")

ggplot(fish_length, aes(x = length_cm, y = YEAR, group = YEAR, fill = YEAR)) + 
  geom_density_ridges(aes(point_fill = YEAR, point_color = YEAR),
                      jittered_points = FALSE, scale = 3.0, alpha = 0.3, point_alpha = 1, lwd = 0.75) +
  geom_vline(xintercept = 61, linetype = "dashed", lwd = 1.25) +
  xlim(35, 90) + ylim("2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010", "2009",
                      "2008", "2007", "2006", "2005", "2004", "2003", "2002", "2001", "2000", 
                      "1999", "1998", "1997") + 
  xlab("Length (cm)") + ylab("Year") +
  theme(legend.position = "none") + facet_wrap(~ survey_type)

dev.off()

#Lengths by Sex and Fishery Type
length_ll <- fish_length %>% filter(survey_type == "Longline", sex != "Unknown")

tiff(filename = "H:/Groundfish/Sablefish/Clarence/ssei_sablefish/figures/ssei_fishery_ll_lengths.tiff",
     width = 8, height = 9, units = "in", res = 600, compression = "lzw")

ggplot(length_ll, aes(x = length_cm, y = YEAR, group = YEAR, fill = YEAR)) + 
  geom_density_ridges(aes(point_fill = YEAR, point_color = YEAR),
                      jittered_points = FALSE, scale = 3.0, alpha = 0.3, point_alpha = 1, lwd = 0.75) +
  geom_vline(xintercept = 61, linetype = "dashed", lwd = 1.25) +
  xlim(35, 90) + ylim("2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010", "2009",
                      "2008", "2007", "2006", "2005", "2004", "2003", "2002", "2001", "2000", 
                      "1999", "1998", "1997") + 
  xlab("Length (cm)") + ylab("Year") +
  theme(legend.position = "none") + facet_wrap(~ sex)

dev.off()

length_pot <- fish_length %>% filter(survey_type == "Pot", sex != "Unknown")

tiff(filename = "H:/Groundfish/Sablefish/Clarence/ssei_sablefish/figures/ssei_fishery_pot_lengths.tiff",
     width = 8, height = 9, units = "in", res = 600, compression = "lzw")

ggplot(length_pot, aes(x = length_cm, y = YEAR, group = YEAR, fill = YEAR)) + 
  geom_density_ridges(aes(point_fill = YEAR, point_color = YEAR),
                      jittered_points = FALSE, scale = 3.0, alpha = 0.3, point_alpha = 1, lwd = 0.75) +
  geom_vline(xintercept = 61, linetype = "dashed", lwd = 1.25) +
  xlim(35, 90) + ylim("2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010", "2009",
                      "2008", "2007", "2006", "2005", "2004", "2003", "2002", "2001", "2000",
                      "1999", "1998", "1997") + 
  xlab("Length (cm)") + ylab("Year") +
  theme(legend.position = "none") + facet_wrap(~ sex)

dev.off()



#LL SURVEY LENGTH FREQUENCIES BY YEAR#
target <- c("Male", "Female")

survey_length <- survey %>% filter(Sex %in% target) %>%
  mutate(length_cm = `Length Millimeters` / 10)

survey_length$Year <- as.factor(survey_length$Year)

tiff(filename = "H:/Groundfish/Sablefish/Clarence/ssei_sablefish/figures/ssei_survey_lengths.tiff",
     width = 8, height = 9, units = "in", res = 600, compression = "lzw")

ggplot(survey_length, aes(x = length_cm, y = Year, group = Year, fill = Year)) + 
  geom_density_ridges(aes(point_fill = Year, point_color = Year),
                      jittered_points = FALSE, scale = 3.5, alpha = 0.3, point_alpha = 1, lwd = 0.75) +
  geom_vline(xintercept = 61, linetype = "dashed", lwd = 1.25) +
  xlim(35, 90) + ylim("2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010",
                      "2009", "2008", "2007", "2006", "2005", "2004", "2003", "2002", "2001", "2000",
                      "1999", "1998", "1997", "1996", "1995", "1994", "1993", "1992", "1991",
                      "1990", "1989", "1988", "1987", "1986") + 
  xlab("Length (cm)") + ylab("Year") +
  theme(legend.position = "none") + facet_wrap(~ Sex)

dev.off()












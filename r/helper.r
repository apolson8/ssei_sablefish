# Libraries, ggplot themes, and user-defined functions used in this project
# Author: Andrew Olson (andrew.olson@alaska.gov)
# Last modified: April 2019

# Libraries ----
library(tidyverse)
library(readxl)
library(ggthemes)
library(extrafont)
library(lubridate)
library(gridExtra)
library(tidyr)
library(padr)
library(anytime)
library(ggridges)

# ggoplot themes ----

loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_bw(base_size=14,base_family='Times New Roman') +
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()))

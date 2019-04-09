# Libraries, ggplot themes, and user-defined functions used in this project
# Author: Andrew Olson (andrew.olson@alaska.gov)
# Last modified: April 2019

# Libraries ----
library(tidyverse)
library(readxl)
library(ggthemes)   
library(extrafont)  # for ggplot theme
library(lubridate)
library(gridExtra)  
library(tidyr)       # data processing
library(padr)        # pads time series with 0s or interpolated values
library(anytime)     # easily deals with any date/time format
library(ggridges)    # length comps
# install.packages("devtools")
# devtools::install_github("ben-williams/FNGr")
library("FNGr")

# ggoplot themes ----

loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_bw(base_size=14,base_family='Times New Roman') +
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()))

##SSEI LL SURVEY AND FISHERY LENGTH FREQUENCIES#
library(tidyverse)
library(lubridate)
library(readxl)
library(ggthemes)
library(gridExtra)
library(extrafont)
library(tidyr)
library(padr)
library(anytime)


##THEMES FOR GRAPHS##
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_bw(base_size=14,base_family='Times New Roman')
          +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()))

#COLOR BLIND PALETTE#
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#IMPORT FISHTICKET DATA FROM ALEX AND SELECT PERMIT FISHERY C61C & C91C#
fishery <- read_excel("H:/Groundfish/Sablefish/Clarence/ssei_sablefish/data/fishery/raw_data/SSEI fishticket data.xlsx")

#IMPORT POT AND LL LOGBOOKS FROM ALEX#
pot_log <- read_excel("H:/Groundfish/Sablefish/Clarence/ssei_sablefish/data/fishery/raw_data/SSEI pot logbook data.xlsx")

ll_log <- read_excel("H:/Groundfish/Sablefish/Clarence/ssei_sablefish/data/fishery/raw_data/SSEI longline logbook data.xlsx")

#TOTAL HARVEST BY YEAR AND PERMIT TYPE#
#AHO BY YEAR IS TAKEN FROM THE MGMT MEMO#
YEAR <- c(1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999,
          2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014,
          2015, 2016, 2017, 2018, 2019)
aho <- c(790000, 790000, 790000, 790000, 790000, 790000, 790000, 790000, 790000, 790000, 790000,
         790000, 790000, 632000, 720000, 696000, 696000, 696000, 696000, 696000, 696000, 696000,
         696000, 696000, 634000, 634000, 583280, 583280, 583280, 536618, 536618, 482956, 516763,
         578774, 607713)


ssei_aho <- data.frame(YEAR, aho)

#MAKE SURE TO FILTER OUT OTHER SPECIES AND TEST FISH HARVEST FROM THE SURVEY#
gear <- c(61, 91)

harv <- fishery %>% group_by(YEAR) %>% filter(SPECIES_CODE == 710 & HARVEST_CODE != 43 & GEAR_CODE %in% gear) %>% 
  full_join(ssei_aho) %>% summarise(total_harvest = sum(ROUND_POUNDS)) %>% 
  mutate(mgmt_type = ifelse(YEAR %in% 1985:1996, "Limited Entry", "Equal Quota Share"))
  

harv

tiff(filename = "H:/Groundfish/Sablefish/Clarence/ssei_sablefish/figures/ssei fishery harvest.tiff",
     width = 9, height = 8, units = "in", res = 600, compression = "lzw")


ggplot(harv, aes(YEAR, total_harvest, fill = mgmt_type)) + 
  geom_bar(stat = "identity", color = "black") +
  geom_line(aes(YEAR, aho), size = 2, color = "black", linetype = "dashed") +
  ylab("Harvest (round lb)") + xlab("Year") +
  scale_x_continuous(breaks = pretty(harv$YEAR , n =5), limits = c(1985, 2020)) +
  scale_y_continuous(label = scales::comma)+
  scale_fill_manual(values= cbPalette) +
  theme(legend.position = c(0.75, 0.85), legend.title = element_blank())

dev.off()

#HARVEST DISTRIBUTION BY STAT-AREA#
harv_prop <- fishery %>% filter(SPECIES_CODE == 710 & HARVEST_CODE != 43 & G_STAT_AREA != 1065 & 
                                  G_MANAGEMENT_AREA_CODE == "SSEI" & YEAR >= 1997) %>%
  mutate(area = ifelse(G_STAT_AREA %in% c(325431, 315431, 325401, 315401), "Dixon Entrance",
                ifelse(G_STAT_AREA %in% c(305431, 305501, 305502, 305503, 315432, 315501,
                                          315502, 315503, 315504, 325433, 325501, 325502,
                                          325503, 325504), "Lower Clarence Strait",
                ifelse(G_STAT_AREA %in% c(305531, 305532, 315531, 315532, 325531, 325532,
                                          325533, 335506, 335534, 335535), "Upper Clarence Strait",
                ifelse(G_STAT_AREA %in% c(315600, 325601, 325602, 325603, 325604, 325631, 325632,
                                          335533, 335601, 335602, 335603, 335632, 335633, 345535,
                                          345604), "Sumner Strait", "Other"))))) %>%
  group_by(YEAR, area) %>% summarise(total_harvest = sum(ROUND_POUNDS), permit_count = n_distinct(CFEC_NO))

#RE-ORDER LEVELS OF FACTORS FROM NORTH TO SOUTH#
harv_prop$area <- factor(harv_prop$area, levels = c("Sumner Strait", "Upper Clarence Strait", "Lower Clarence Strait",
                                                    "Dixon Entrance"))

#MAKE NON-CONFIDENTIAL BY FILTERING OUT NUMBER OF PERMIT HOLDERS LESS THAN 3#
harv_prop_nonconf <- harv_prop %>% filter(permit_count >= 3)

tiff(filename = "figures/SSEI Fishery Harvest Distribution.tiff",
     width = 8, height = 8, units = "in", res = 600, compression = "lzw")

ggplot(harv_prop_nonconf, aes(YEAR, total_harvest, fill = area)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = cbPalette) + 
  ylab("Total Harvest (round lbs)") + xlab("Year") +
  scale_x_continuous(breaks = seq(1996, 2019, 2), limits = c(1996, 2019)) +
  scale_y_continuous(labels = scales::comma, breaks = seq(0, 700000, 100000)) +
  theme(legend.position = c(0.75, 0.88), legend.title = element_blank())

dev.off()

#POT FISHERY CPUE#

pot_ticketlbs <- fishery %>% filter(GEAR == "Pot") %>% group_by(YEAR) %>%
  summarise(total_pounds = sum(ROUND_POUNDS), permit_count = n_distinct(CFEC_NO))

pot_effort <- pot_log %>% group_by(YEAR) %>%
  summarise(total_effort = sum(NUMBER_OF_POTS))

#JOIN FISHTICKET LBS AND LOGBOOK EFFORT TO CALCULATE CPUE#
pot_cpue <- pot_ticketlbs %>% full_join(pot_effort) %>% 
  mutate(cpue = total_pounds / total_effort)
#unsure how to incoporate sd for CPUE since data is outputted via ALEX with harvest summaries by stat-area

pot_cpue_nonconf <- pot_cpue %>% filter(permit_count >= 3)

tiff(filename = "figures/Pot Fishery CPUE.tiff",
     width = 8, height = 7, units = "in", res = 600, compression = "lzw")

ggplot(pot_cpue_nonconf, aes(YEAR, cpue)) + geom_line(size = 1.25) + ylab("CPUE (round lb/pot)") +
  geom_point(size = 4, color = "dodgerblue1") + xlab("Year") + ylim (0, 100) +  
  scale_x_continuous(breaks=seq(1996, 2019, 2), limits = c(1995, 2019))

dev.off()

#LL SURVEY CPUE#
#Code borrowed from J. Sullivan's NSEI sablefish assessment
#Import SSEI LL survey hook accounting form
hooks <- read.csv("data/survey/raw_data/ssei survey hook accounting.csv")

#Rename column headers
plyr::rename(hooks, replace = c("Year" = "year", "Project" = "project", "G.Stat.Area" = "g_stat_area",
                                "Set.No" = "set_no", "Subset.Condition" = "subset_condition", 
                                "Subset.Condition.Code"= "subset_condition_code",
                                "Hooks...Total" = "no_hooks", "Hooks...Baited" = "hooks_bait",
                                "Hooks...Bare" = "hooks_bare", "Hooks...Invalid" = "hooks_invalid",
                                "Hooks...Uknown" = "hooks_unknown", "Hooks...Hagfish.Slime" = "hooks_slime", 
                                "Sablefish" = "hooks_sablefish")) -> hooks

valid <- c(1, 3)

srv_cpue <- hooks %>% 
  filter(year >= 1998 & #soak time was standardized to at least 3 hours in 1997 prior to that it was 1 hour 
           # Mike Vaughn 2018-03-06: Sets (aka subsets with 12 or more invalid hooks are subset condition code "02" or invalid)
           subset_condition_code %in% valid, no_hooks != 0) %>% 
  mutate(Year = factor(year),
         Stat = factor(g_stat_area),
         #standardize hook spacing (Sigler & Lunsford 2001, CJFAS) changes in 
         #hook spacing. pers. comm. with aaron.baldwin@alaska.gov: 1995 & 1996 -
         #118 in; 1997 - 72 in.; 1998 & 1999 - 64; 2000-present - 78". This is
         #different from KVK's code (he assumed 3 m before 1997, 2 m in 1997 and
         #after)
         hooks_bare = ifelse(is.na(hooks_bare), 0, hooks_bare),
         hooks_bait = ifelse(is.na(hooks_bait), 0, hooks_bait),
         hooks_invalid = ifelse(is.na(hooks_invalid), 0, hooks_invalid),
         no_hooks = no_hooks - hooks_invalid,
         std_hooks = ifelse(year %in% c(1998, 1999), 2.2 * no_hooks * (1 - exp(-0.57 * (64 * 0.0254))),
                                           2.2 * no_hooks * (1 - exp(-0.57 * (78 * 0.0254)))),
  
         sablefish_retained = replace(hooks_sablefish, is.na(hooks_sablefish), 0), # make any NAs 0 values
         std_cpue = sablefish_retained/std_hooks #*FLAG* this is NPUE, the fishery is a WPUE
         # raw_cpue = sablefish_retained/no_hooks
  ) 


srv_sum <- srv_cpue %>% 
  group_by(year) %>% 
  # mutate(
  #   #mean annual cpue
  #   annual_cpue = mean(NPUE)
  summarise(annual_cpue = round(mean(std_cpue), 2),
            # nn = length(std_cpue),
            sdev = sd(std_cpue),
            # std_error = sdev / sqrt(nn),
            CIupper = annual_cpue + (sdev * 2),
            CIlower = annual_cpue - (sdev * 2))


# figures
ggplot(data = srv_sum) +
  geom_line(aes(year, annual_cpue), lwd = 1) +
  geom_point(aes(year, annual_cpue), size = 4, color = "dodgerblue1") +
  # geom_ribbon(aes(year, ymin = CIlower, ymax = CIupper),
  geom_ribbon(aes(year, ymin = annual_cpue - sdev, ymax = annual_cpue + sdev),
              alpha = 0.3, fill = "gray") +
  ylab("Survey CPUE (number of sablefish per hook)\n") +
  xlab("Year") +
  scale_y_continuous(breaks = seq(0.0, 0.3, 0.05)) +
  scale_x_continuous(breaks=seq(1998, 2018, 2), limits = c(1998, 2018))

ggsave("figures/ssei_ll_survey_cpue.png", 
       dpi=300, height=7, width=8, units="in")

#LL Fishery CPUE 
#SQL data is requested from Justin Daily (comm fish programmer) which allocates
#harvest data by lbs per set 
# Logbook/CPUE data  ----
#Import sablefish_lbs_set file
ll_set <- read_excel("data/fishery/raw_data/ssei longline sablefish lbs per set.xlsx")

fish_eff <- ll_set %>% mutate(date = anydate(TIME_SET), 
                              julian_day = yday(date),
                              time_set = anytime(TIME_SET),
                              time_hauled = anytime(TIME_HAULED),
                              soak = difftime(time_hauled, time_set, units = "hours"),
                              Gear = factor(LONGLINE_SYSTEM_CODE),
                              Hook_size = HOOK_SIZE, 
                              hook_space = HOOK_SPACING, #*FLAG* - check that hook_space is in inches
                              Size = factor(as.numeric(gsub("[^0-9]", "", Hook_size))),
                              no_hooks = NUMBER_OF_HOOKS,
                              sable_lbs_set = SABLE_LBS_PER_SET) %>% 
select(year = YEAR, trip_no = TRIP_NO, Adfg = ADFG_NO, Spp_cde = TRIP_TARGET, date, julian_day, 
       soak, Gear = LONGLINE_SYSTEM_CODE, Hook_size, Size, 
       hook_space, Stat = G_STAT_AREA, no_hooks, depth = AVERAGE_DEPTH_METERS, 
       sets = EFFORT_NO, sable_lbs_set, start_lat = START_LATITUDE_DECIMAL_DEGREES,
       start_lon = START_LONGITUDE_DECIMAL_DEGREE)


# Read in data, standardize cpue, etc.
fish_eff %>% filter(!is.na(date) & !is.na(hook_space) & !is.na(sable_lbs_set) &
                    !is.na(start_lon) & !is.na(start_lon) & !is.na(soak) & !is.na(depth) &
                    !is.na(Hook_size) & Hook_size != "MIX" &
                    soak > 0 & !is.na(soak) & # soak time in hrs
                    #julian_day > 226 & # if there were special projects before the fishery opened
                    #this was excluded since it crashed the code
                    no_hooks < 15000)  %>% # 15000 in Kray's scripts - 14370 is the 75th percentile
  mutate(Year = factor(year), 
         Gear = factor(Gear),
         Adfg = factor(Adfg),
         Gear = ifelse(Gear == "06", "AB",
                ifelse(Gear == "01", "CS",
                ifelse(Gear == "02" , "CS",
                ifelse(Gear == "05", "CS", "Other")))),
         Hook_size = factor(Hook_size),
         # standardize hook spacing (Sigler & Lunsford 2001, CJFAS), 1 m = 39.37 in
         std_hooks = 2.2 * no_hooks * (1 - exp(-0.57 * (hook_space / 39.37))), 
         std_cpue = sable_lbs_set / std_hooks,
         # dummy varbs, for prediction with random effects
         dum = 1, 
         dumstat = 1) %>% 
  #sets are already pre-filtered via the SQL data request so only sablefish target trips are included
  group_by(year, trip_no) %>% 
  mutate(no_sets = n_distinct(sets)) %>% 
  group_by(year) %>% 
  mutate(
    total_vessels = n_distinct(Adfg),
    # Total unique trips per year
    total_trips = n_distinct(trip_no)) %>% 
  ungroup() -> fsh_cpue

axis <- tickr(fsh_cpue, year, 3)

fsh_cpue %>% 
  select(year, Vessels = total_vessels, Trips = total_trips) %>% 
  gather(Variable, Count, -year) %>% 
  distinct() %>%   
  ggplot(aes(x = year, y = Count)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~ Variable, ncol = 1, scales = "free") +
  labs(x = "", y = "") +
  ylim(0, NA)

ggsave(paste0("figures/fishery_tripandvessel_trends_1997_", YEAR, ".png"), 
       dpi=300, height=6, width=5, units="in")



# Nominal CPUE ----

fsh_cpue %>% 
  group_by(year) %>% 
  summarise(annual_cpue = mean(std_cpue),
            sdev = sd(std_cpue),
            n = length(std_cpue),
            se = sdev / (n ^ (1/2)),
            var = var(std_cpue),
            cv = sdev / annual_cpue,
            upper = annual_cpue + (2 * se),
            lower = annual_cpue - (2 * se)) -> fsh_sum 

#Figure
ggplot(data = fsh_sum) +
  geom_line(aes(year, annual_cpue), lwd = 1) +
  geom_point(aes(year, annual_cpue), size = 4, color = "dodgerblue1") +
  # geom_ribbon(aes(year, ymin = CIlower, ymax = CIupper),
  geom_ribbon(aes(year, ymin = annual_cpue - sdev, ymax = annual_cpue + sdev),
              alpha = 0.3, fill = "gray") +
  ylab("Standardized Longline Fishery CPUE (round lb/hook)") +
  xlab("Year") +
  scale_y_continuous(breaks = seq(0.0, 0.9, 0.1)) +
  scale_x_continuous(breaks=seq(1998, 2018, 2), limits = c(1997, 2018)) 


ggsave("figures/ssei_ll_fishery_cpue.png", 
              dpi=300, height=7, width=8, units="in")


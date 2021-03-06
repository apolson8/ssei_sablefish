
# SSEI sablefish analysis for management memo. 
# Includes: survey and fishery CPUE and summary of biological data
# Authors:  Andrew Olson (andrew.olson@alaska.gov); and Rhea Ehresmann (rhea.ehresmann@alaska.gov) 
# Code adapted from J.S. NSEI Sablefish assessment: Jane Sullivan (jane.sullivan@alaska.gov)
# Last modified: April 10, 2020 for testing 

# set up ----
source('r/helper.r') 

# Create figure and output folders
YEAR <- 2020 # assessment year
fig_path <- paste0('figures/', YEAR) # folder to hold all figures for a given year
dir.create(fig_path) # creates YEAR subdirectory inside figures folder
output_path <- paste0('output/', YEAR) # output and results
dir.create(output_path) 

# data ----

# harvest by year and permit type AHO from management memo
ssei_aho <- data.frame(year = c(1985:2019),
                       aho = c(rep(790000, 13), 632000, 720000, rep(696000, 9),
                               634000, 634000, 583280, 583280, 583280, 536618, 
                               536618, 482956, 516763, 578774, 590349))  #need to update with AHO for 2020

# Get all fish ticket data from ALEX query for sablefish in SSEI management area
# and exclude fish tickets from test fishery = 43 
read_xlsx("data/fishery/raw_data/SSEI fishticket data new.xlsx") %>%  
  rename_all(tolower) %>% 
  filter(species_code == 710, harvest_code != 43) -> fishery_df

# Data from ALEX query for pot logbook data
read_xlsx("data/fishery/raw_data/SSEI pot logbook data new.xlsx") %>%  
  rename_all(tolower) -> pot_log_df

# Data from ALEX query for longline logbook data
read_xlsx("data/fishery/raw_data/SSEI longline logbook data new.xlsx") %>% 
  rename_all(tolower) -> ll_log_df

# Data from SQL request to Justin Daily - fishery logbook
read_xlsx("data/fishery/raw_data/ssei longline sablefish lbs per set new.xlsx") %>% 
  rename_all(tolower) -> ll_set_df

# Data from OceanAK query for longline survey hook accounting in SSEI 
read_xlsx("data/survey/raw_data/ssei survey hook accounting new.xlsx") %>% 
  rename_all(tolower) -> srv_cpue

# Data from OceanAK query for longline survey bio data in SSEI 
read_xlsx("data/survey/raw_data/SSEI LL survey bio data new.xlsx") %>% 
  rename_all(tolower) -> svy_bio_df

# Data from ALEX query for port sampling data in SSEI 
read_xlsx("data/fishery/raw_data/SSEI port sampling data new.xlsx") %>% 
  rename_all(tolower) -> fish_bio_df


# harvest by year and permit type ----

fishery_df %>% 
  filter(gear_code %in% c(61, 91), year > 1985) %>% 
  full_join(ssei_aho) %>% 
  group_by(year) %>% 
  summarise(total_harvest = sum(round_pounds),
            aho = mean(aho)) %>% 
  mutate(mgmt_type = ifelse(year %in% 1985:1996, "Limited Entry", "Equal Quota Share")) -> harvest

write_csv(harvest, paste0(output_path, "/harvest.csv")) # save output

xaxis <- FNGr::tickr(harvest, year, 3)

ggplot(harvest, aes(year, total_harvest)) + 
  geom_bar(stat = "identity", aes(fill = mgmt_type)) +
  geom_line(aes(y = aho), linetype = 3, size = 1) +
  ylab("Harvest (round lbs)\n") + xlab("\nYear") +
  scale_fill_grey() + # use grey-scale for the report
  #scale_fill_manual(values= cbPalette) + scale_color_manual(values = cbPalette) +
  scale_x_continuous(breaks = xaxis$breaks, labels=xaxis$labels) +
  scale_y_continuous(label = scales::comma)+
  theme(legend.position = c(0.75, 0.85), legend.title = element_blank())

ggsave(paste0(fig_path, '/ssei_fishery_harvest.png'), width = 6.5, height = 5, units = "in", dpi = 200)

# Harvest distribution by area ---- not in report, look at distribution

fishery_df %>% 
  filter(g_stat_area != 1065, 
         g_management_area_code == "SSEI", year >= 1997) %>%
  mutate(Area = case_when(g_stat_area %in% c(325431, 315431, 325401, 315401) ~ "Dixon Entrance",
                          g_stat_area %in% c(305431, 305501, 305502, 305503, 315432, 315501,
                                             315502, 315503, 315504, 325433, 325501, 325502,
                                             325503, 325504) ~ "Lower Clarence Strait",
                          g_stat_area %in% c(305531, 305532, 315531, 315532, 325531, 325532,
                                             325533, 335506, 335534, 335535) ~ "Upper Clarence Strait",
                          g_stat_area %in% c(315600, 325601, 325602, 325603, 325604, 325631, 325632,
                                             335533, 335601, 335602, 335603, 335632, 335633, 345535,
                                             345604) ~ "Sumner Strait",
                          TRUE ~ "Other")) %>%
  group_by(year, Area) %>% 
  summarise(total_harvest = sum(round_pounds), 
            permit_count = n_distinct(cfec_no)) %>% 
  mutate(Area = factor(Area, 
                       levels = c("Sumner Strait", "Upper Clarence Strait", 
                                  "Lower Clarence Strait", "Dixon Entrance"))) %>% 
  filter(permit_count >= 3) -> area_harvest 

write_csv(area_harvest, paste0(output_path, "/harvest_byarea.csv")) # save output

ggplot(area_harvest, aes(year, total_harvest, fill = Area)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_grey() + 
  ylab("Total Harvest (round lbs)\n") + 
  xlab("\nYear") +
  scale_x_continuous(breaks = xaxis$breaks, labels=xaxis$labels) +
  scale_y_continuous(labels = scales::comma, breaks = seq(0, 700000, 100000)) +
  theme(legend.position = c(0.75, 0.88), legend.title = element_blank())

ggsave(paste0(fig_path,"/SSEI_Fishery_Harvest_Distribution.png"), width = 6.5, 
       height = 6, units = "in", dpi = 200)

# pot fishery cpue ----
  
# Confidentiality
fishery_df %>%
  filter(gear == 'Pot') %>%
  group_by(year) %>% 
  summarize(n_tickets = n_distinct(ticket_no), 
         n_permits = n_distinct(cfec_no)) %>% 
  filter(n_permits < 3) -> omit_yrs
  
# Effort data currently summarized by stat area. We want trip totals for all of
# SSEI, so sum by trip_no
pot_log_df %>% 
  filter(year != omit_yrs$year) %>% 
  group_by(year, trip_no) %>% 
  summarize(round_pounds = sum(pounds_of_sablefish),
            n_pots = sum(number_of_pots)) %>% 
  mutate(cpue = round_pounds / n_pots) %>% 
  summarise(sd = sd(cpue),
            cpue = mean(cpue),
            n = n(),
            se = sd / sqrt(n)) %>% 
  mutate(ll = cpue - 2 * se,
         ul = cpue + 2 * se) -> pot_cpue 

write_csv(pot_cpue, paste0(output_path, "/pot_cpue.csv")) # save output

pot_cpue %>% ggplot(aes(year, cpue)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = ll, ymax = ul), alpha=0.2) +
  ylab("CPUE (round lbs/pot)\n") +
  xlab('\nYear') +
  scale_x_continuous(breaks = xaxis$breaks, labels = xaxis$labels) +
  theme(plot.margin = unit(c(0.5,1,0.5,0.5), "cm")) +
  expand_limits(y = 0)

ggsave(paste0(fig_path,"/pot_fishery_cpue.png"), width = 6.5, 
       height = 5, units = "in", dpi = 200)

# ll survey cpue ---- 

# standardize hook spacing (Sigler & Lunsford 2001, CJFAS) changes in hook
# spacing. pers. comm. with aaron.baldwin@alaska.gov: 1995 & 1996 - 118 in; 1997
# - 72 in.; 1998 & 1999 - 64 in; 2000-present - 78 in. This is different from KVK's
# code (he assumed 3 m before 1997, 2 m in 1997 and after) soak time was
# standardized to at least 3 hours in 1997 prior to that it was 1 hour Mike
# Vaughn 2018-03-06: Sets (aka subsets with 12 or more invalid hooks are subset
# condition code "02" or invalid)

srv_cpue <- srv_cpue %>% 
  dplyr::select(year, trip_no = `trip no`, set = `set no`, skate = `subset no`,
                skate_condition_cde = `subset condition code`, 
                Stat = `g stat area`, bare = `hooks - number bare`,
                bait = `hooks - number with bait`, invalid = `hooks - number invalid`,
                no_hooks = `hooks - total number`, sablefish)

# we need to review raw data - looks like there are a few subsets with more than
# 12 invalid hooks that are not invalid

# data checks 

# TODO: these should be changed to condition code 2. 
srv_cpue %>% filter(skate_condition_cde %in% c(1,3) & invalid > 12) 
srv_cpue <- srv_cpue %>% # Fix manually for now
  mutate(skate_condition_cde = ifelse(skate_condition_cde %in% c(1,3) & invalid > 12, 
                                      2, skate_condition_cde)) 

srv_cpue %>% filter(no_hooks < 0) # there should be none


srv_cpue %>% filter(year > 1997 & c(is.na(no_hooks) | no_hooks == 0)) # there should be none, there is one.
# year trip_no   set skate skate_condition_cde   Stat  bare  bait invalid no_hooks sablefish
# <dbl>   <dbl> <dbl> <dbl>               <dbl>  <dbl> <dbl> <dbl>   <dbl>    <dbl>     <dbl>
#   1  2016       2     6    16                   2 325431    NA    NA      NA        0         0

# TODO this needs to be fixed in database: bare, bait, invalid should all be 0,
# skate_condition_cde should be 2. Fixed manually for now:
srv_cpue <- srv_cpue %>% 
  mutate(skate_condition_cde = ifelse(year > 1997 & c(is.na(no_hooks) | no_hooks == 0), 2, skate_condition_cde))

# Get subset for cpue analysis, standardize hooks
srv_cpue <- srv_cpue %>% 
  filter(year >= 1998, 
         skate_condition_cde %in% c(1, 3)) %>% 
  replace_na(list(bare = 0, bait = 0, invalid = 0, sablefish = 0)) %>% 
  mutate(no_hooks = no_hooks - invalid, # remove invalid hooks
         std_hooks = ifelse(year %in% c(1998, 1999), 2.2 * no_hooks * (1 - exp(-0.57 * (64 * 0.0254))),
                            2.2 * no_hooks * (1 - exp(-0.57 * (78 * 0.0254)))))

# Data set is currently at skate level (each row is a skate). CPUE should be
# calculated at the set level (skates within a set are expected to be highly
# correlated, not independent)
srv_cpue %>% 
  group_by(year, trip_no, set) %>%
  dplyr::summarise(bare = sum(bare),
         bait = sum(bait),
         sablefish = sum(sablefish),
         set_hooks = sum(std_hooks),
         set_cpue = sablefish / set_hooks) %>%
  ungroup() %>% 
  mutate(trip_set_id = paste0(trip_no, "_", set)) %>% 
  group_by(year) %>% 
  dplyr::summarise(n_set = length(unique(trip_set_id)),
                   cpue = mean(set_cpue),
                   sd = round(sd(set_cpue), 4),
                   se = round(sd / sqrt(n_set), 4)) -> srv_cpue

write_csv(srv_cpue, paste0(output_path, "/llsurvey_cpue.csv")) # save output

# Percent change in compared to a ten year rolling average
srv_cpue %>% 
  filter(year > YEAR - 10 & year <= YEAR) %>% 
  mutate(lt_mean = mean(cpue),
         perc_change_lt = (cpue - lt_mean) / lt_mean * 100,
         eval_lt = ifelse(perc_change_lt < 0, "decrease", "increase")) %>% 
  filter(year == max(srv_cpue$year)) -> srv_lt
srv_lt

# Percent change from last year
srv_cpue %>% 
  filter(year >= max(srv_cpue$year) - 1 & year <= max(srv_cpue$year)) %>%
  select(year, cpue) %>% 
  
  mutate(year2 = ifelse(year == max(srv_cpue$year), "thisyr", "lastyr")) %>% 
  dcast("cpue" ~ year2, value.var = "cpue") %>% 
  mutate(perc_change_ly = (thisyr - lastyr) / lastyr * 100,
         eval_ly = ifelse(perc_change_ly < 0, "decreased", "increased")) -> srv_ly
srv_ly

# Figure
axis <- tickr(srv_cpue, year, 3)
ggplot(data = srv_cpue) +
  geom_point(aes(x = year, y = cpue)) +
  geom_line(aes(x = year, y = cpue)) +
  geom_ribbon(aes(year, ymin = cpue - sd, ymax = cpue + sd),
              alpha = 0.2) +
  scale_x_continuous(breaks = axis$breaks, labels = axis$labels) + 
  lims(y = c(0, 0.3)) +
  labs(x = NULL, y = "Survey CPUE (number per hook)\n") 

ggsave(paste0(fig_path,"/ssei_ll_survey_cpue.png"), 
       dpi = 300, width = 6.5, height = 5, units = "in")

# Show how/where old cold was wrong:
tst <- srv_cpue$cpue
(tmp <- mean(log(tst+1)))
exp(tmp-1) # see how the scale is now similar to the figs you had? this transformation isn't needed and was done incorrectly
# If you ever wanted a log transformation adding 1 (i.e. if data included 0),
# here's how to do it correctly:
(tmp <- log(mean(tst)+1))
exp(tmp)-1 # will equal mean(tst)
mean(tst)

# ll fishery cpue ----

ll_set_df %>% 
  select(year, trip_no, adfg = adfg_no, Spp_cde = trip_target, time_set,
         time_hauled, Gear = longline_system_code, hook_size,  
         hook_spacing, Stat = g_stat_area, no_hooks = number_of_hooks, 
         depth = average_depth_meters, 
         sets = effort_no, sable_lbs_set = sable_lbs_per_set, 
         start_lat = start_latitude_decimal_degrees,
         start_lon = start_longitude_decimal_degree) %>% 
  mutate(date = anydate(time_set),
         julian_day = yday(date),
         time_set = anytime(time_set),
         time_hauled = anytime(time_hauled),
         soak = difftime(time_hauled, time_set, units = 'hours'),
         Gear = factor(Gear),
         Gear = case_when(Gear == "06" ~ "AB",
                          Gear %in% c("01", "02", "05") ~ "CS",
                          TRUE ~ "Other"),
         Hook_size = factor(hook_size),
         Size = factor(as.numeric(gsub("[^0-9]", "", hook_size))),
         Year = factor(year),
         ADFG = factor(adfg),
         std_hooks = 2.2 * no_hooks * (1 - exp(-0.57 * (hook_spacing / 39.37))), 
         std_cpue = sable_lbs_set / std_hooks,
         dum = 1, 
         dumstat = 1) %>% 
  filter(!is.na(date), !is.na(hook_spacing), !is.na(sable_lbs_set),
         !is.na(start_lon), !is.na(start_lon), !is.na(soak), !is.na(depth),
         !is.na(hook_size), hook_size != "MIX", soak > 0) %>% 
  group_by(year, trip_no) %>% 
  mutate(no_sets = n_distinct(sets)) %>% 
  group_by(year) %>% 
  mutate(total_vessels = n_distinct(adfg),
         total_trips = n_distinct(trip_no)) %>% 
  ungroup() -> fishery_cpue 

# Trends in number of total trips and vessels participating in the fishery
fishery_cpue %>% 
  select(year, Vessels = total_vessels, Trips = total_trips) %>% 
  gather(Variable, Count, -year) %>% 
  distinct() %>%   
  ggplot(aes(year, Count)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~ Variable, ncol = 1, scales = "free") +
  labs(x = "\nYear", y = "") +
  scale_x_continuous(breaks = xaxis$breaks, labels = xaxis$labels) +
  expand_limits(y = 0) +
  theme(plot.margin = unit(c(0.5,1,0.5,0.5), "cm"))

ggsave(paste0(fig_path, "/fishery_trip_vessel_trends_1997_", YEAR, ".png"), width = 6.5, 
       height = 8, units = "in", dpi = 200)

# nominal cpue ----

fishery_cpue %>% 
  group_by(year) %>% 
  summarise(annual_cpue = mean(std_cpue),
            sdev = sd(std_cpue),
            n = length(std_cpue),
            se = sdev / sqrt(n()),
            var = var(std_cpue),
            cv = sdev / annual_cpue,
            upper = annual_cpue + (2 * se),
            lower = annual_cpue - (2 * se)) -> fish_sum

write_csv(fish_sum, paste0(output_path, "/llfishery_cpue.csv")) # save output

xaxis <- FNGr::tickr(fishery_cpue, year, 3)
fish_sum %>% ggplot(aes(year, annual_cpue)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin = annual_cpue - sdev, ymax = annual_cpue + sdev),
              alpha = 0.2) +
  labs(x = NULL, y = "Longline Fishery CPUE (lb/hook)\n") +
  scale_x_continuous(breaks=xaxis$breaks, labels=xaxis$labels) +
  expand_limits(y = 0) + ylim(0, 0.9) 

ggsave(paste0(fig_path, "/ssei_ll_fishery_cpue.png"), width = 6.5, 
       height = 5, units = "in", dpi = 200)

# Percent change in fishery nominal cpue compared to a ten year rolling average
fish_sum %>% 
  filter(year > max(fish_sum$year) - 10) %>% 
  mutate(lt_mean = mean(annual_cpue),
         perc_change_lt = (annual_cpue - lt_mean) / lt_mean * 100)

# Percent change in fishery nominal cpue from last year
fish_sum %>% 
  filter(year >= max(fish_sum$year) - 1) %>%
  select(year, annual_cpue) %>% 
  dcast("annual_cpue" ~ year) -> perc_ch
names(perc_ch) <- c("cpue", "last_year", "this_year") 
perc_ch %>% mutate(perc_change_ly = (`this_year` - `last_year`) / `last_year` * 100)

# Age comps ----

rec_age <- 2 # age recruiting to fishery or survey
plus_group <- 25 # plus group, lump all the old fish into on age

rbind(
  # ll survey
  svy_bio_df %>% 
    filter(age != "NA", 
           sex %in% c("Male", "Female"), 
           `age readability` %in% c("Very Sure", "Comfortably Sure", "Fairly Sure"),
           age >= rec_age) %>% 
    mutate(Source = "Longline survey") %>% 
    select(Source, year, Sex = sex, age),
  # ll gear
  fish_bio_df %>% 
    filter(project_code == 2, age != "NA", 
           sample_type == "Random", sex_code %in% c(1, 2), 
           age_readability_code %in% c(1, 2, 3)) %>% 
    mutate(Sex = case_when(sex_code == 1 ~ "Male",
                         sex_code == 2 ~ 'Female',
                         TRUE ~ 'Other'),
           Source = "Longline fishery") %>% 
    select(Source, year, Sex, age),
  fish_bio_df %>% 
    filter(project_code == 17, age != "NA", 
           sample_type == "Random", sex_code %in% c(1, 2), 
           age_readability_code %in% c(1, 2, 3)) %>% 
    mutate(Sex = case_when(sex_code == 1 ~ "Male",
                           sex_code == 2 ~ 'Female',
                           TRUE ~ 'Other'),
           Source = "Pot fishery") %>% 
    select(Source, year, Sex, age)) %>% 
  filter(age >= rec_age) %>% 
  mutate(age = ifelse(age >= plus_group, plus_group, age)) %>% 
  count(Source, year, Sex, age) %>% 
  group_by(Source, year, Sex) %>% 
  mutate(proportion = round( n / sum(n), 5)) %>% 
  tidyr::complete(age = rec_age:plus_group, nesting(Source, Sex, year), fill = list(n = 0, proportion = 0)) %>% 
  arrange(Source, year, Sex, age) %>% 
  ungroup() %>% 
  mutate(label = case_when(Source == "Longline survey" ~ "llsrv",
                           Source == "Longline fishery" ~ "llfsh",
                           Source == "Pot fishery" ~ "potfsh" )) -> agecomp_df
#agecomp_df %>% group_by(Source) %>% distinct(year) %>% View()
  
# Check that they sum to 1
agecomp_df %>% 
  group_by(Source, Sex, year) %>% 
  summarise(sum(proportion)) %>% View

# Output age comp sample sizes and proportions
write_csv(agecomp_df, paste0(output_path, "/age_comps.csv"))

# Function to plot functions

plot_age <- function(data = agecomp_df,
                     src = NULL) {
  
  data <- data %>% filter(label == src) 
  data <-  data %>% complete(year = seq(min(data$year), max(data$year), 1))
  xaxis <- FNGr::tickr(data, year, 5)
  yaxis <- FNGr::tickr(data, age, 5)
  
  p <- ggplot(data = na.omit(data),
              aes(x = year, y = age, size = proportion)) + #*FLAG* could swap size with proportion_scaled
    geom_point(shape = 21, fill = "black", colour = "black") +
    scale_size(range = c(0, 4)) +
    facet_wrap(~ Sex) +
    labs(x = "\nYear", y = "Observed age\n") +
    guides(size = FALSE) +
    scale_x_continuous(breaks = xaxis$breaks, labels = xaxis$labels) +
    scale_y_continuous(breaks = yaxis$breaks, labels = yaxis$labels)
  
  
  print(p)
  ggsave(plot = p, paste0(fig_path, "/agecomp_", src, ".png"), dpi=300, height=5, width=7.5, units="in")
  
}

plot_age(data = agecomp_df, src = "llsrv")
plot_age(data = agecomp_df, src = "llfsh")
plot_age(data = agecomp_df, src = "potfsh")

# lengths ----

fish_bio_df %>% 
  filter(sex_code!=0) %>% 
  mutate(Sex = case_when(sex_code == 1 ~ "Male",
                         sex_code == 2 ~ 'Female',
                         TRUE ~ 'Other'),
         length = length_millimeters / 10,
         survey_type = ifelse(project_code==2, 'Longline', 'Pot')) -> fish_lengths

fish_lengths %>% 
  ggplot(aes(length, year, group = year, fill = year)) + 
  geom_density_ridges(aes(point_fill = year, point_color = year),
                      alpha = 0.3) +
  #geom_vline(xintercept = 61, linetype = 4) +  #skip this line as it pertains more to NSEI - J.S. 04/01/2020
  xlim(35, 90) + 
  xlab("\nLength (cm)") + 
  ylab("Year\n") +
  scale_y_reverse() +
  theme(legend.position = "none") + 
  facet_wrap(~ survey_type)

ggsave(paste0(fig_path, "/ssei_fishery_lengths.png"), width = 6.5, 
       height = 8, units = "in", dpi = 200)

# length by gear types ----
# longline
fish_lengths %>% 
  filter(survey_type == "Longline", Sex != 'Other') %>% 
  ggplot(aes(length, year, group = year, fill = year)) + 
  geom_density_ridges(aes(point_fill = year, point_color = year),
                       scale = 3, alpha = 0.3) +
  #geom_vline(xintercept = 61, linetype = 3) + #skip this line as it pertains more to NSEI - J.S. 04/01/2020
  xlim(35, 90) +
  xlab("\nLength (cm)") + 
  ylab("Year\n") +
  scale_y_reverse() +
  theme(legend.position = "none") +
  facet_wrap(~ Sex)

ggsave(paste0(fig_path, "/ssei_fishery_ll_lengths_sex.png"), width = 6.5, 
       height = 8, units = "in", dpi = 200)

# pot
fish_lengths %>% 
  filter(survey_type == "Pot", Sex != 'Other') %>% 
  ggplot(aes(length, year, group = year, fill = year)) + 
  geom_density_ridges(aes(point_fill = year, point_color = year),
                      scale = 3, alpha = 0.3) +
  #geom_vline(xintercept = 61, linetype = 3) + #skip this line as it pertains more to NSEI - J.S. 04/01/2020
  xlim(35, 90) +
  xlab("\nLength (cm)") + 
  ylab("Year\n") +
  scale_y_reverse() +
  theme(legend.position = "none") +
  facet_wrap(~ Sex)

ggsave(paste0(fig_path, "/ssei_fishery_pot_lengths_sex.png"), width = 6.5, 
       height = 8, units = "in", dpi = 200)

# survey lengths ----
svy_bio_df %>% 
  filter(sex %in% c('Male', 'Female')) %>% 
  mutate(length = `length millimeters` / 10) %>% 
  ggplot(aes(length, year, group = year, fill = year)) + 
    geom_density_ridges(aes(point_fill = year, point_color = year),
                        scale = 3, alpha = 0.3) +
    #geom_vline(xintercept = 61, linetype = 3) + #skip this line as it pertains more to NSEI - J.S. 04/01/2020
    xlab("Length (cm)") + ylab("Year") +
  scale_y_reverse() +
  theme(legend.position = "none") +
  xlim(35, 90) + 
  facet_wrap(~ sex)

ggsave(paste0(fig_path, "/ssei_survey_lengths.png"), width = 6.5, 
       height = 8, units = "in", dpi = 200)






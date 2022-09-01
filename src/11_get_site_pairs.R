# 
# Purpose: To get a dataframe of all connected/unconnected sites
# Author: Jake Diamond
# Date: July 20, 2020
# 

# Load libraries
library(readxl)
library(lubridate)
library(tidyverse)
library(tidytable) #to run things a bit faster for the larger dataframes

# Load hourly data
df <- readRDS(file.path("data", "all_time_series_for_analysis.RDS"))

# Read in watershed info
ws_meta <- read_excel(file.path("data", "metadata", "Loire_headwaters_data_share.xlsx"),
                      sheet = "watershed_meta") %>%
  rename(Site = site)

# Ugh, need to get better meta for Site and site code
x <- distinct(readRDS(file.path("data", "headwaters_data_clean.RDS")), 
                      Site, site_code)

# get dataframe for connectivity
df_con <- df %>%
  left_join(x) %>%
  select(Site, site_code, datetime, DO, temp, light) %>% 
  left_join(ws_meta) %>%
  mutate(year = year(datetime)) %>%
  left_join(read_excel(file.path("data", "metadata", "longitudinal_meta.xlsx"))) #all connected sites

# Create two separate metadata for paired sites
df_meta1 <- df_con %>%
  ungroup() %>%
  distinct(site_code, area_km2, long_use) %>%
  select(site1 = site_code, area1 = area_km2, long1 = long_use)
df_meta2 <- df_con %>%
  ungroup() %>%
  distinct(site_code, area_km2, long_use) %>%
  select(site2 = site_code, area2 = area_km2, long2 = long_use)

# Dataframe of all connected site vs site combos
df_pos_con <- select(ungroup(df_con) %>% group_by(watershed), site1 = site_code) %>%
  distinct() %>%
  bind_cols(select(ungroup(df_con), site2 = site_code) %>%
              distinct()) %>%
  expand(site1, site2, crossing(year = 2019:2020)) %>% # get all sites against eachother
  filter(site1 != site2) %>% #don't want to compare data against itself
  ungroup() %>%
  left_join(df_meta1) %>% 
  left_join(df_meta2) %>%
  group_by(watershed, year) %>%
  filter(area2 > area1, str_detect(long2, long1)) #this removes any site pairs that are not connected longitudinally

# Dataframe of all unconnected site vs site combos
df_pos_un <- select(ungroup(df_con), site1 = site_code) %>%
  distinct() %>%
  bind_cols(select(ungroup(df_con), site2 = site_code) %>%
              distinct()) %>%
  expand(site1, site2, crossing(year = 2019:2020)) %>% # get all sites against eachother
  filter(site1 != site2) %>% #don't want to compare data against itself
  ungroup() %>%
  left_join(df_meta1) %>% 
  left_join(df_meta2) %>%
  group_by(year) %>%
  filter(!str_detect(long2, long1)) #this removes any site pairs that are connected longitudinally

# Gather all of the data, for unconnected sites
df_pos_un <- df_pos_un %>%
  mutate.(site1data = pmap.(.l = list(site1, year),
                          .f = function(x, z) filter.(ungroup(df_con), site_code == x,
                                                     year == z) %>%
                            select.(datetime, DO, temp, light)),
         site2data = pmap.(.l = list(site2, year),
                          .f = function(x, z) filter.(ungroup(df_con), site_code == x,
                                                     year == z) %>%
                            select.(datetime, DO, temp, light)))

# Create a column of the paired data between each site combination
df_pos_un2 <- df_pos_un %>%
  mutate.(paired = pmap.(.l = list(site1data, site2data),
                       .f = function(x, y) left_join.(x, y,
                                                     by = "datetime")))

# Gather all of the data, for connected sites
df_pos_con <- df_pos_con %>%
  mutate.(site1data = pmap.(.l = list(site1, year),
                            .f = function(x, z) filter.(ungroup(df_con), site_code == x,
                                                        year == z) %>%
                              select.(datetime, DO, temp, light)),
          site2data = pmap.(.l = list(site2, year),
                            .f = function(x, z) filter.(ungroup(df_con), site_code == x,
                                                        year == z) %>%
                              select.(datetime, DO, temp, light)))

# Create a column of the paired data between each site combination
df_pos_con2 <- df_pos_con %>%
  mutate.(paired = pmap.(.l = list(site1data, site2data),
                         .f = function(x, y) left_join.(x, y,
                                                        by = "datetime")))

# Get rid of O row data, nested format
pairs_un <- select.(df_pos_un2, -site1data, -site2data) %>%
  unnest.(paired) %>%
  arrange.(site1, site2, datetime) %>%
  group_by(site1, site2, year) %>%
  filter.(!(all(is.na(DO.x)) | all(is.na(DO.y)))) %>%
  ungroup() %>%
  group_by(site1, site2, year)

pairs_con <- select.(df_pos_con2, -site1data, -site2data) %>%
  unnest.(paired) %>%
  arrange.(site1, site2, datetime) %>%
  group_by(site1, site2, year) %>%
  filter.(!(all(is.na(DO.x)) | all(is.na(DO.y)))) %>%
  ungroup() %>%
  group_by(site1, site2, year)

# Get it in long format
p_con_l <- pairs_con %>%
  pivot_longer.(cols = DO.x:light.y)

p_un_l <- pairs_un %>%
  pivot_longer.(cols = DO.x:light.y)

p_con_l <- mutate.(p_con_l, 
                  type = str_extract(name, "[^.]+"))

p_un_l <- mutate.(p_un_l, 
             type = str_extract(name, "[^.]+"))

# Save data
saveRDS(p_con_l, file.path("data", "unconnected_DO_data_long.RDS"))
saveRDS(p_un_l, file.path("data", "connected_DO_data_long.RDS"))
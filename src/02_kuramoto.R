# 
# Purpose: To calculate kuramoto among all sites and only connected sites
# Author: Jake Diamond
# Date: July 20, 2020
# 

# Load libraries
library(lubridate)
library(readxl)
library(broom)
library(seewave)
library(patchwork)
library(tidyverse)

# Load hourly data
df <- readRDS(file.path("data", "all_time_series_for_analysis.RDS")) %>%
  ungroup() %>%
  mutate(site = tolower(Site)) %>%
  select(-Site)

# Source the functions
source(file.path("src", "01_kuramoto_functions.R"))

# Kuramoto on hourly data
df_kur_h <- df %>%
  select(site, datetime, DO, temp, lux, light) %>%
  ungroup() %>%
  mutate(week = week(datetime), year = year(datetime)) %>%
  pivot_longer(cols = -c(site, year, datetime, week)) %>%
  group_by(name, year, week) %>%
  rename(time = datetime) %>%
  nest() %>%
  mutate(kuramoto = map_progress(data, kur_fun)) 

df_kur <- df_kur_h %>%
  select(-data) %>%
  ungroup() %>%
  dplyr::filter(kuramoto != "Error") %>%
  unnest(kuramoto) %>%
  rename(datetime = time) %>%
  mutate(date = date(datetime),
         sd = sqrt(1-1/nsites)/nsites,
         r = if_else(between(datetime, ymd_h("2019110500"), ymd_h("2020030100")), NA_real_, r)) %>%
  group_by(name, date, year) %>%
  summarize(r = mean(r),
            sd = mean(sd))

saveRDS(df_kur, file.path("results", "kuramoto_all_sites.RDS"))
# Connected sites ---------------------------------------------------------
# Load connected site data
df_con <- readRDS(file.path("data", "connected_data_long.RDS"))

# Kuramoto on hourly data
df_con_kur <- df_con %>%
  ungroup() %>%
  group_by(watershed, site1, site2, type, year, week) %>%
  arrange(name, time) %>%
  rename(site = name) %>%
  nest() %>%
  mutate(kuramoto = map_progress(data, kur_fun)) 

# Get into format for plotting
df_con_kur_p <- df_con_kur %>%
  select(-data) %>%
  ungroup() %>%
  dplyr::filter(kuramoto != "Error") %>%
  unnest(kuramoto) %>%
  rename(datetime = time) %>%
  mutate(date = date(datetime),
         sd = sqrt(1-1/nsites)/nsites,
         r = if_else(between(datetime, ymd_h("2019110500"), ymd_h("2020030100")), NA_real_, r)) %>%
  group_by(type, date, year) %>%
  summarize(r = mean(r),
            sd = mean(sd))
saveRDS(df_con_kur_p, file.path("results", "kuramoto_connected.RDS"))

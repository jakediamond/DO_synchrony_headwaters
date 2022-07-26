# 
# Purpose: To grab all DO and light data in clean dataframes
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
  filter(kuramoto != "Error") %>%
  unnest(kuramoto) %>%
  rename(datetime = time) %>%
  mutate(date = date(datetime),
         sd = sqrt(1-1/nsites)/nsites,
         r = if_else(between(datetime, ymd_h("2019110500"), ymd_h("2020030100")), NA_real_, r)) %>%
  group_by(name, date, year) %>%
  summarize(r = mean(r),
            sd = mean(sd))



p_all_daily <- ggplot(data = filter(df_kur, name != "lux") %>% drop_na(),
                      aes(x = date,
                          y = r,
                          color = name,
                          group = interaction(name, year))) + 
  geom_point(alpha = 0.4, size = 0.9) +
  # geom_ribbon(aes(ymax = r +sd, ymin = r-sd, alpha = 0.4)) +
  facet_grid(~year, scales = "free_x", space = "free_x") +
  stat_smooth(se = FALSE, span = 0.15) +
  scale_color_manual(values = c("black", "#ff0000", "#f2ad00"),
                     breaks = c("DO", "temp", "light"),
                     # name = "variable",
                     labels = c("DO", "temp.", "light")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%m/%y") +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.25)) +
  theme_bw(base_size = 11) +
  theme(legend.position = c(0.45, 0.23),
        strip.text = element_blank(),
        strip.background = element_blank(),
        # legend.box.background = element_rect(color = "black", fill = "transparent"),
        legend.background = element_rect(color = "black", fill = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        legend.margin = margin(t = 0, unit='cm'),
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        axis.title.x = element_blank()) +
  labs(x = "",
       y = "synchrony (-)",
       title = "all sites",
       color = NULL)
p_all_daily

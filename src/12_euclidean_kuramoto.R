# 
# Purpose: To do Euclidean distance version of kuramoto
# Author: Jake Diamond
# Date: 1 September 2022
# 

# Load libraries
library(readxl)
library(lubridate)
library(sf)
library(tidyverse)
library(tidytable) #to run things a bit faster for the larger dataframes

# Load hourly data
df_un <- readRDS(file.path("results", "kuramoto_unconnected_pairs.RDS"))
df_con <- readRDS(file.path("results", "kuramoto_connected.RDS"))

# Get it all together
df <- select.(df_un, site1, site2, year, type, datetime, r) %>%
  mutate.(con = "unconnected",
         date = date(datetime)) %>%
  summarize.(r = mean(r, na.rm = T),
             .by = c(site1, site2, year, type, date, con)) %>%
  bind_rows(df_con %>% select(site1, site2 = site_code, date, r) %>%
              mutate(year = year(date),
                     con = "connected"))

# Load metadata of lat long
df_meta <- distinct(readRDS(file.path("data", "headwaters_data_clean.RDS")), 
                         Site, site_code) %>%
  left_join(distinct(readRDS(file.path("data", "all_time_series_for_analysis.RDS")),
                     Site, latitude, longitude)
            )

# Get distances between all points
pts <- st_as_sf(df_meta, coords = c("longitude", "latitude"),
                crs = 4326)
dists <- st_distance(pts, by_element = FALSE)
df_dist <- as_tibble(matrix(dists, nrow=42)) %>%
  rownames_to_column("siteno1") %>%
  pivot_longer(-siteno1, names_to = "siteno2", values_to = "eucdist_m") %>%
  filter(eucdist_m != 0) %>%
  mutate(siteno2 = as.numeric(str_extract(siteno2, "[0-9]+")),
         siteno1 = as.numeric(siteno1))

# Get the site names on there
df_dist2 <- distinct(df, site1, site2) %>%
  right_join(df_meta %>%
               mutate(site1 = site_code,
                      siteno1 = row_number())) %>%
  right_join(df_meta %>%
               mutate(site2 = site_code,
                      siteno2 = row_number()) %>%
               select(site2, siteno2) , by = "site2") %>%
  right_join(df_dist) %>%
  drop_na()

# Now get that included on the kuramoto df
df <- left_join(df, df_dist2)

# Bin the data to plot it
df_bins <- filter(df, type!="lux",
                  year == 2020,
                  month(date) > 2) %>%
  group_by(type, con) %>%
  mutate(brk = ntile(eucdist_m, 100)) %>%
  group_by(brk, type, con) %>%
  summarize(rm = mean(r, na.rm = T),
            rse = sd(r, na.rm = T) / sqrt(n()),
            d = mean(eucdist_m, na.rm = T)) %>%
  drop_na()

p <- ggplot(data = df_bins, 
       aes(x = -d,
           y = rm,
           linetype = con),
       color = "blue")  +
  # geom_point() +
  # geom_errorbar(aes(ymin = rm - rse, ymax = rm + rse)) + 
  facet_grid(cols = vars(type)) +
  scale_linetype_discrete(name = "flow-connectivity") +
  theme_bw() +
  geom_smooth() +
  # geom_smooth(method = "lm", se = FALSE) +
  # ggpubr::stat_regline_equation(label.y = 0.64,
  #   aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"))) +
  labs(x = "Euclidean distance (m)",
       y = expression("synchrony, "*italic(r)~"(-)"))

p

ggsave(plot = p, 
       filename = file.path("results", "supplementary", "euclidean_distance.png"),
       dpi = 1200,
       width = 18.4,
       height = 12,
       units = "cm")

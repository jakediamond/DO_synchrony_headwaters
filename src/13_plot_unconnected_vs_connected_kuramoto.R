# 
# Purpose: To compare synchrony between connected/unconnected sites via the connectivity ratio
# Author: Jake Diamond
# Date: July 1, 2021
# 

# Load libraries
library(lubridate)
library(tidyverse)

# Load data
df_un <- readRDS(file.path("results", "kuramoto_unconnected_pairs.RDS"))
df_dists <- readRDS(file.path("data", "distance_ratios.RDS"))
df_con <- readRDS(file.path("results", "kuramoto_connected.RDS")) %>%
  left_join(df_dists) %>%
  ungroup() %>%
  mutate(dist_ratio = dist_ratio * 3 / 0.7)

# Get the unconnected data by the second site, daily mean r
df_un_site <- df_un %>%
  mutate(r = if_else(between(datetime, ymd_h("2019110500"), ymd_h("2020030100")), NA_real_, r),
         date = date(datetime)) %>%
  group_by(type, date, year, site2) %>%
  summarize(r = mean(r))

# Combine with unconnected data, based on the same date and second site
df_p <- df_con %>%
  left_join(select(df_un_site, date, r_un = r, site_code = site2)) %>%
  mutate(pair = paste(site1, site_code)) 

# Plot the data
p <- ggplot(filter(df_p, type %in% c("DO", "temp")),
       aes(x = dist_ratio,
           y = r)) +
  facet_wrap(~type) +
  stat_smooth(aes(linetype = "connected"), level = 0.999) +
  stat_smooth(data = filter(df_p, type %in% c("DO", "temp")),
              aes(x = dist_ratio,
                  y = r_un,
                  linetype = "un-connected"), level = 0.999) +
  scale_linetype_manual(name = "flow-connectivity",
                        labels = c("connected", "un-connected"),
                        values = c("solid", "dashed")) +
  geom_vline(xintercept = 1) +
  scale_x_log10()+
  annotation_logticks(sides = "b") +
  theme_bw(base_size = 10) +
  theme(legend.position = c(0.85, 0.15),
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
        strip.background = element_blank()) +
  labs(x = "connectivity ratio (-)",
       y = "synchrony (-)",
       title = "")
p
ggsave(plot = p,
       filename = file.path("results", "supplementary", "connectivity_unconnected_comparison.png"),
       dpi = 1200,
       width = 18.4,
       height = 12,
       units = "cm")
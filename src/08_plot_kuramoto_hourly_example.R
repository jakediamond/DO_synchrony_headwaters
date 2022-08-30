# 
# Purpose: To show hourly timeseries of kuramoto 
# Author: Jake Diamond
# Date: July 20, 2020
# 

# Load libraries
library(lubridate)
library(patchwork)
library(tidyverse)

# Load hourly data
df <- readRDS(file.path("data", "all_time_series_for_analysis.RDS")) %>%
  ungroup() %>%
  mutate(site = tolower(Site)) %>%
  select(-Site)

# Source the functions
source(file.path("src", "01_kuramoto_functions.R"))

# Get subset of data for the Loise catchment
df_l <-  ungroup(df) %>%
  mutate(date = date(datetime),
         strahler = round(strahler),
         week = week(date),
         year = year(date)) %>%
  dplyr::filter(watershed == "Loise",
                (week == 15 & year == 2020) |
                  (week == 22 & year == 2020) |
                  (week == 28 & year == 2020) |
                  (week == 41 & year == 2019)) %>%
  select(site, strahler, datetime, week, year, DO)


# Calculate Kuramoto for those dates
df_kur <- select(df_l, -strahler) %>%
  rename(time = datetime, value = DO) %>%
  group_by(year, week) %>%
  nest() %>%
  mutate(kuramoto = map(data, possibly(.f = kur_fun, otherwise = "Error"))) %>%
  select(-data) %>%
  unnest(cols = kuramoto)

# Get ready to plot
df_ex <- df_l %>%
  mutate(date = date(datetime)) %>%
  # dplyr::filter(date %in% c(ymd("20200411"), ymd("20200531"),
  #                           ymd("20200712"), ymd("20191008"))) %>%
  mutate(light = case_when(month(date) == 4 ~ "high light synchrony", 
                           month(date) %in% c(5,6) ~ "low light synchrony",
                           month(date) == 7 ~ "low light synchrony",
                           month(date) == 10 ~ "high light synchrony"),
         conn = case_when(month(date) == 4 ~ "high connectivity", 
                          month(date) %in% c(5,6) ~ "high connectivity",
                          month(date) == 7 ~ "low connectivity",
                          month(date) == 10 ~ "low connectivity")) %>%
  mutate(light = fct_relevel(light, "high light synchrony", "low light synchrony"),
         conn = fct_relevel(conn, "low connectivity", "high connectivity")) %>%
  group_by(site, light, conn) %>%
  arrange(datetime) %>%
  mutate(time = row_number() %% 24)

df_r <- ungroup(df_kur) %>%
  mutate(date = date(time)) %>%
  mutate(light = case_when(month(date) == 4 ~ "high light synchrony", 
                           month(date) %in% c(5,6) ~ "low light synchrony",
                           month(date) == 7 ~ "low light synchrony",
                           month(date) == 10 ~ "high light synchrony"),
         conn = case_when(month(date) == 4 ~ "high connectivity", 
                          month(date) %in% c(5,6) ~ "high connectivity",
                          month(date) == 7 ~ "low connectivity",
                          month(date) == 10 ~ "low connectivity")) %>%
  mutate(light = fct_relevel(light, "high light synchrony", "low light synchrony"),
         conn = fct_relevel(conn, "low connectivity", "high connectivity")) %>%
  group_by(light, conn) %>%
  arrange(time) %>%
  mutate(time = row_number() %% 24)

# Dates to plot
dates <- c(ymd("20200411"), ymd("20200531"), ymd("20200712"), ymd("20191012"))

# Plot time series
p_ts <- ggplot(data = dplyr::filter(df_ex, date %in% dates)) +
  geom_line(aes(x = time, y = DO, group = site, color = as.factor(strahler)),
            size = 1, alpha = 0.7)+
  facet_grid(light ~ conn, scales = "free", switch="y") + 
  # scale_x_continuous(breaks = seq(0,23, 6)) +
  scale_color_viridis_d(name = "Strahler order", 
                        guide = guide_legend(title.position = "top", 
                                             title.hjust = 0.5)) +
  theme_bw(base_size = 9) +
  labs(y = "DO (% saturation)",
       x = "time of day (h)") +
  theme(strip.background = element_blank(),
        axis.title.y = element_blank(),
        legend.position = c(0.8, 0.12),
        legend.key.size = unit(0.3, "cm"),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.background = element_rect(color = "black"),
        legend.direction = "horizontal",
        strip.placement = "outside",
        # axis.title = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())
p_ts

# Plot r
p_r <- ggplot(data = dplyr::filter(df_r, date %in% dates)) +
  geom_line(aes(x = time, y = r), color = "red", size = 1)+
  facet_grid(light ~ conn, scales = "free") + 
  theme_bw(base_size = 9) +
  scale_y_continuous(position = "right", limits = c(0,1), breaks = seq(0,1,0.5)) + 
  labs(y = expression(DO[sat]~"synchrony")) +
  theme(axis.text.y = element_text(color = "red"),
        axis.ticks.y = element_line(color = "red"),
        axis.title.y = element_text(color = "red"),
        strip.background = element_blank(),
        strip.text = element_blank(),
        legend.position = "none",
        plot.tag = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "transparent")) +
  cowplot::panel_border(remove = TRUE)
p_r

# Plot with kuramoto
layout <- c(area(t = 1, l = 1, b = 5, r = 5),
            area(t = 1, l = 1, b = 5, r = 5))
p_grid_ws <- p_ts + p_r + plot_layout(design = layout)
p_grid_ws <- egg::tag_facet(p_grid_ws, x = 0.2, open = "", close = "", fontface = 1)
p_grid_ws <- p_grid_ws + theme(strip.text = element_blank())
p_grid_ws


ggsave(plot = p_grid_ws,
       filename = file.path("results","figures","Figure6.svg"),
       dpi = 1200,
       width = 9.2,
       height = 9.2,
       units = "cm")

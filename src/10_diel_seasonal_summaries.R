# -------------------------------------
# Author: Jake Diamond
# Purpose: Get general diel stats for time series across seasons
# Date: July 10, 2020
# -------------------------------------

# Load libraries
library(lubridate)
library(tidyverse)

# Load DO, light, and temp data
df <- readRDS(file.path("data", "all_time_series_for_analysis.RDS"))

# Quick summary
df_sum <- ungroup(df) %>%
  mutate(date = date(datetime),
         stra = round(strahler)) %>%
  group_by(stra, date, site) %>%
  summarize(across(.cols = c(DO), 
                   list(mean = ~mean(.x, na.rm = T), 
                        amp = ~max(.x, na.rm = T) - min(.x, na.rm = T)),
                   .names = "{.col}_{.fn}"
  )
  )

df_sum2 <- ungroup(df_sum) %>%
  mutate(month = month(date),
         year = year(date),
         pd = case_when(
           between(month, 3, 4) ~ "spring",
           between(month, 8, 9) & year == 2020 ~ "dry summer",
           between(month, 10, 11) ~ "leaf fall",
           between(month, 5, 9) ~ "summer")
  ) %>%
  filter_all(all_vars(!is.infinite(.)), site != "charpassonne moulin marcel") %>%
  group_by(stra, pd) %>%
  summarize(across(where(is.numeric),
                   list(mean = ~mean(.x, na.rm = T),
                        sd = ~sd(.x, na.rm = T)),
                   .names = "{.col}_{.fn}")
  )
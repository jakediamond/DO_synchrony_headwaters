# 
# Purpose: To show sensitivity of kuramoto to noise, magnitude, and phase shift
# Author: Jake Diamond
# Date: July 1, 2022
# 

# Load libraries
library(lubridate)
library(patchwork) #for pretty plots later
library(tidyverse)

# Set seed
set.seed(42)

# Get functions
source(file.path("src", "00_lowpass_function.R"))
source(file.path("src", "01_kuramoto_functions.R"))


# Noise and phase effects on Kuramoto -------------------------------------
# Generate 7 days of hourly data data with some noise
# Choose phase lag from 0 to pi
# A phase lag of pi should result in r = 0 (complete asynchrony)
# A phase lag of pi/2 should result in r = 0.7
phase_lag <- pi/4
n_days <- 7
t <- seq(from = 0, to = n_days*24, by = 1)
DO1 <- 100 + 20*(sin((t-6)*2*pi/24 - phase_lag)) + 8*rnorm(t) #maybe a 4-5th order river, peak at 18h
DO2 <- 70 + 2*(sin((t-6)*2*pi/24)) + 2*rnorm(t) #maybe a 1-2 order river, peaks at 12pm

# Take a quick look
plot(DO1, type = "l", ylim = c(30,130))
lines(DO2, col = "red")

# Get data in typical format used for calculating kuramoto
dat <- as_tibble(rbind(cbind(time = t, value = DO1, site = 1),
               cbind(time = t, value = DO2, site = 2))) %>%
  mutate(time = ymd_h("2020070100") +hours(time))

# Do the kuramoto function on the raw data
r_test <- kur_fun(dat)

# Apply the lowpass filter to the data
dat_filt <- dat %>%
  group_by(site) %>%
  nest() %>%
  mutate(filt = map(data, ~lowpass_fun(., 0.1))) %>% #you can vary the cutoff frequency
  select(-data) %>%
  unnest(filt) %>%
  mutate(site = if_else(site == 1, "site 1" ,"site 2"))

# Do the kuramoto on the filtered data
dat2 <- select(dat_filt, site, time, value = filtered)
r_test_filt <- kur_fun(dat2)

# Plot of time series comparison of filtered and unfiltered data
p_ts_comp <- dat_filt %>%
  select(!value_an_int) %>%
  pivot_longer(value:filtered) %>%
  ggplot(aes(x = time,
             y = value,
             color = name,
             alpha = name)) +
  geom_line(size = 1.5) +
  scale_alpha_manual(values = c(1, 0.7)) +
  scale_color_manual(values = c("orange", "blue")) +
  facet_wrap(~site, scales = "free", ncol = 1) +
  scale_x_datetime(date_breaks = "1 day",
                   limits = c(min(dat2$time), max(dat2$time))) +
  labs(x = "", y = expression(DO[sat]~"(%)")) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())

# Get both kuramotos together for plotting
all_r <- left_join(r_test,
                     select(r_test_filt, time, filtered = r)) %>%
  select(time, value = r, filtered) %>%
  pivot_longer(cols = c(value, filtered), values_to = "r")

# Summarize average synchrony over the week
rraw = round(mean(r_test$r), 2)
rfilt = round(mean(r_test_filt$r), 2)

# Plot of the two kuramoto timeseries
p_r_comp <- ggplot(data = all_r,
       aes(x = time,
           y = r,
           color = name,
           alpha = name)) +
  geom_line(size = 1.5) +
  scale_alpha_manual(values = c(1, 0.7)) +
  scale_color_manual(values = c("orange", "blue")) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%H:%M",
                   limits = c(min(dat2$time), max(dat2$time))) +
  annotate("text", x = ymd_h(2020070212), y = 0.2,
           color = "orange", 
           label = paste0("bar(r)==",rfilt),
           parse = T) +
  annotate("text", x = ymd_h(2020070212), y = 0.1, 
           color = "blue", 
           label = paste0("bar(r)==",rraw),
           parse = T) +
  theme_bw() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        axis.title.x = element_blank())

# Final plot
p <- (p_ts_comp / p_r_comp) + plot_layout(guides = 'collect')
p
ggsave(plot = p, 
       filename = file.path("results", "supplementary", "filt_vs_not.png"),
       dpi = 1200,
       width = 18.4,
       height = 12,
       units = "cm")

# Number of site and phase effects on Kuramoto ----------------------------
# create a grid of treatments 
trts <- expand.grid(nsites = seq(2, 30, 1), # number of sites (range used in this work)
                    phase_lag = seq(0, pi, pi/6) # mean phase lag
                    )

# Function to generate n sites of data with p average phase shift
dat_fun <- function(n, p){
  set.seed(42)
  n_days = 7 # days to model
  t = seq(from = 0, to = n_days*24, by = 1) #hourly time sequence
  sites = 1:n
  shift = sites %% 2 * rnorm(n, p, p/8) #every other site apply the random shift to
  d = sites %>%
    setNames(sites) %>%
    map2_dfc(shift, ~100 + 20*(sin((t-6)*2*pi/24 + .y))) %>% #random phase shift
    bind_cols(time = t) %>%
    mutate(time = ymd_h("2020070100") + hours(time)) %>%
    pivot_longer(1:all_of(n), names_to = "site")
}

# Apply data function to the treatments
trts <- trts %>%
  mutate(dat = pmap(list(nsites, 
                         phase_lag),
                    dat_fun))

# Calculate Kuramoto r for each treatment, takes a bit of time
trts <- trts %>%
  mutate(r = map(dat, kur_fun))

# Get in good format
r_trts <- trts %>% 
  select(-dat, -nsites) %>%
  unnest(r)

# Average behavior
r_avg <- r_trts %>%
  group_by(nsites, phase_lag) %>%
  summarize(rmean = mean(r))

# Plot of overall results
p_lag_site <- ggplot(data = r_avg,
       aes(x = nsites,
           y= phase_lag,
           fill = rmean)) +
  geom_tile() +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, pi, pi / 6),
                     labels = expression(0, frac(pi, 6), frac(pi, 3), frac(pi, 2),
                                         frac(2*pi, 3), frac(5*pi, 6), pi)) +
  scale_fill_viridis_c(name = expression(bar(r)),
                       option = "E",
                       limits = c(0, 1)) +
  labs(y = "mean phase lag (rad)",
       x = "number of sites") +
  theme(legend.position = "right")

p_lag_site
ggsave(plot = p_lag_site, 
       filename = file.path("results", "supplementary", "sites_and_phase_effects.png"),
       dpi = 1200,
       width = 18.4,
       height = 12,
       units = "cm")


# Some kind of benchmark test using the pairs

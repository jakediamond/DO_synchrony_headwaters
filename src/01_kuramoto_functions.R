# 
# Purpose: To define functions for calculating kuramoto order parameter across sites
# Author: Jake Diamond
# Date: July 20, 2020
# 

# Load libraries
library(lubridate)
library(zoo)
library(wsyn)
library(seewave)
library(tidyverse)
library(progress)

# Kuramoto order function
kuramoto_order_parameter <- function(spacetime){
  # Returns the Kuramoto order parameter for an NxT array.
  #   Parameters:
  #   spacetime: NxT array (N: number of nodes, T: time)
  #              Each row of represents a timeseries of the corresponding node.
  #   Returns:
  #   r: Global order parameter (Scalar number)
  sum_theta = colSums(exp(0 + 1i*spacetime)) #every time step has a sum of all vectors
  r = abs(sum_theta) / (1*nrow(spacetime)) #abs returns the length of a vector from the origin to a complex value plotted in the complex plane
  return(r)
}

# Simple function to extract phase from the results of the seewave::ifreq function
getphase <- function(data){
  data$p[,2]
}

# Function to apply kuramoto function to data and extract for each time step
kur_fun <- function(data){
  # frequency of data (Hz)
  freq = 1 / time_length(data$time[2] - data$time[1])
  
  # limits of acceptable NA lengths
  lim = if(time_length(data$time[2] - data$time[1]) < 86400) {1} else {24} #1 days, depends on if hourly or daily freq
  
  # Get data frame with no NAs, each column is a site's time series
  x = data %>%
    pivot_wider(names_from = site, values_from = value) %>%
    arrange(time) %>%
    dplyr::select(where(~sum(is.na(.x)) < (86400 * freq * lim))) %>% #remove sites that have too many NA values
    dplyr::filter(if_any(where(is.numeric))) %>% # for light data to remove 0s at nighttime across all sites
    zoo::na.trim(is.na= "all", maxgap = 6) %>% # trim ends with NA
    mutate(across(where(is.numeric), 
                  ~imputeTS::na_interpolation(.x, option = "stine"))) #interpolate NA
  
  # Scale and center and box cox transform
  xmat = select(x, -time) %>% as.matrix() %>% t()
  times = 1:ncol(xmat)
  xmatclean = wsyn::cleandat(xmat, times, clev = 5)$cdat
  xclean = t(xmatclean) %>% as_tibble()
  # plot(xmatclean[4,])
  
  # Calculate instantaneous phases by site
  xp = xclean %>%
    pivot_longer(cols = everything(), names_to = "site", values_to = "value") %>%
    nest_by(site) %>%
    summarize(phase = list(seewave::ifreq(data$value, 
                                          f = freq, 
                                          phase = TRUE, 
                                          plot = FALSE)),
              .groups = "keep")
  
  # number of sites used
  nsites = length(unique(xp$site))
  
  # Instantaneous phases in simple format for estimation of Kuramato
  xf = xp %>%
    summarize(y = map(phase, ~getphase(.))) %>%
    unnest(cols = c(y)) %>%
    ungroup() %>%
    group_by(site) %>%
    mutate(no = row_number()) %>%
    ungroup() %>%
    pivot_wider(values_from = y, names_from = no) %>%
    column_to_rownames("site")
  
  rx = kuramoto_order_parameter(xf)
  
  # as dataframe for clean plotting, summarized by day
  df_x = select(x, time) %>% 
    bind_cols(r = rx, nsites = nsites)
}

map_progress <- function(.x, .f, ...) {
  .f <- purrr::as_mapper(.f, ...)
  pb <- progress::progress_bar$new(total = length(.x), force = TRUE)
  
  f <- function(...) {
    pb$tick()
    .f(...)
  }
  purrr::map(.x, possibly(f, otherwise = "Error") , ...)
}


# 
# df_x %>%
#   mutate(sd = sqrt(1-1/nsites)/nsites) %>%
#   ggplot(aes(x = time,
#            y = r)) +
#   geom_point() +
#   geom_line() +
#   geom_ribbon(aes(ymax = r +sd, ymin = r-sd, alpha = 0.4))

# 
# kur_conn_fun <- function(data){
#   # frequency of data (Hz)
#   freq = 1 / time_length(data$time[3] - data$time[1])
#   
#   # limits of acceptable NA lengths (3 hours or 3 days)
#   lim = if(time_length(data$time[3] - data$time[1]) < 86400) {3/24} else {3} 
#   
#   # Get data frame with no NAs, each column is a site's time series
#   x = data %>%
#     # select(-date) %>%
#     # filter(value != 0) %>%
#     pivot_wider(names_from = name, values_from = value) %>%
#     arrange(time) %>%
#     filter(if_any(where(is.numeric))) %>% # for light data to remove nighttime
#     zoo::na.trim(is.na= "all", maxgap = 6) %>% # trim ends with NA
#     mutate(across(where(is.numeric), ~imputeTS::na_interpolation(.x, option = "stine"))) #interpolate NA
#   
#   # Names of the sites used
#   sites = colnames(x[,-1])
#   
#   # Subtract mean of time series to normalize
#   x[,2] = scale(x[, 2], scale = FALSE)
#   x[,3] = scale(x[, 3], scale = FALSE)
#   # Calculate instantaneous phases by site
#   xp = x %>%
#     pivot_longer(cols = -c(time), names_to = "Site", values_to = "value") %>%
#     nest_by(Site) %>%
#     summarize(phase = list(seewave::ifreq(data$value, f = freq, phase = TRUE, plot = FALSE)))
#   
#   # Instantaneous phases in simple format for estimation of Kuramato
#   xf = xp %>%
#     summarize(y = map(phase, ~getphase(.))) %>%
#     unnest(cols = c(y)) %>%
#     ungroup() %>%
#     group_by(Site) %>%
#     mutate(no = row_number()) %>%
#     ungroup() %>%
#     pivot_wider(values_from = y, names_from = no) %>%
#     column_to_rownames("Site")
#   
#   rx = kuramoto_order_parameter(xf)
#   
#   # as dataframe for clean plotting, summarized by day
#   df_x = select(x, time) %>% bind_cols(r = rx)
# }
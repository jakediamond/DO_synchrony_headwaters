# 
# Purpose: To do regression on kuramoto for DO
# Author: Jake Diamond
# Date: July 20, 2020
# 

# Load libraries
library(lubridate)
library(scales)
library(patchwork)
library(tidyverse)

# Load hourly kuramoto data between connected sites
df <- readRDS(file.path("data", "model_data.RDS"))

# Filter for good data
df_mod <- df %>%
  dplyr::filter(year(date) == 2020, month(date) > 2)

# Summary by bins, 100 breaks
df_bins <- df_mod %>%
  mutate(brk = ntile(lndr, 100)) %>%
  group_by(brk) %>%
  summarize(rse_light = sd(light, na.rm = T) / sqrt(n()),
            rse_DO = sd(DO, na.rm = T) / sqrt(n()),
            rtemp = mean(temp, na.rm = T),
            rDO = mean(DO, na.rm = T),
            rlight= mean(light, na.rm = T),
            dr = mean(lndr, na.rm = T),
            n = n(),
            wt = 1/rse_light) %>%
  drop_na()


modbinl <- lm(rDO ~ scale(rlight), data = df_bins)
summary(modbinl)
AIC(modbinl)
plot(modbinl)

modbind <- lm(rDO ~ scale(dr), data = df_bins)
summary(modbind)
AIC(modbind)
plot(modbind)

anova(modbind, modbinl)

modbind <- lm(rDO ~ scale(dr), data = df_bins)
summary(modbind)
AIC(modbind)

modbin0 <- lm(rDO ~ scale(dr) * scale(rlight), data = df_bins)
summary(modbin0)
AIC(modbin0)
plot(modbin0)
r2_fun(modbin0)

modbin0t <- lm(rtemp ~ scale(rlight) * scale(dr), data = df_bins)
summary(modbin0t)
AIC(modbin0t)
r2_fun(modbin0t)
modbinlt <- lm(rtemp ~ scale(rlight), data = df_bins)
summary(modbinlt)
AIC(modbinlt)
modbindt <- lm(rtemp ~ scale(dr), data = df_bins)
summary(modbindt)
AIC(modbindt)
# Define an r2 function for robust IRWLS regression 
r2_fun <- function(x){
  SSe <- sum((x$resid)^2);  
  observed <- x$resid+x$fitted;  
  SSt <- sum((observed-mean(observed))^2);  
  value <- 1-SSe/SSt;  
  return(value);  
}



# Test something
df_brks <- df_mod %>%
  # group_by(site1, site_code) %>%
  arrange(date) %>%
  mutate(bin = row_number() %% 10) %>%
  group_by(bin) %>%
  nest() %>%
  mutate(moddo = map(data, ~MASS::rlm(DO ~ scale(light) * scale(lndr), data = .)),
         modt = map(data, ~MASS::rlm(temp ~ scale(light) * scale(lndr), data = .)),
         tidydo = map(moddo, broom::tidy),
         tidyt = map(modt, broom::tidy),
         r2do = map(moddo, r2_fun),
         r2t = map(modt, r2_fun)) %>%
  select(-data, -moddo, -modt) %>%
  unnest(cols = c(tidydo, r2do, tidyt, r2t), names_sep = ".") %>%
  select(-tidyt.term) %>%
  group_by(tidydo.term) %>%
  summarize(across(where(is.numeric), mean))


x = pluck(df_brks2, 2, 500)
plot(x$lndr, x$DO)
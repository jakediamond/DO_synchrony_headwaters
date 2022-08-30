# 
# Purpose: To plot kriged heat map of DO synchrony drivers
# Author: Jake Diamond
# Date: July 20, 2020
# 

# Load libraries
library(kriging)
library(metR)
library(scales)
library(tidyverse)

# Load data
df <- readRDS(file.path("data", "data_for_plotting_kriging.RDS"))

# Initial plot of heat map, just to automatically get nicely binned data (n=20)
p1 <- ggplot(data = df,
             aes(x = dr, y = light, z = DO)) +
  scale_x_log10(expand = expansion(mult = c(0, 0))) +
  stat_summary_2d(fun = "median", bins = 20) +
  scale_fill_viridis_c(limits = c(0.0, 1), oob = scales::squish,
                       option = "inferno",
                       name = expression(atop(DO[sat],"synchrony")),
                       guide = guide_colorbar(
                         direction = "horizontal",
                         title.position = "top")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  labs(x = "connectivity ratio (-)",
       y = "light synchrony (-)") +
  theme_bw(base_size = 10) +
  theme(panel.grid = element_blank(),
        legend.position = c(0.88, 0.18),
        plot.tag.position = c(0.16, 0.87),
        legend.key.size = unit(0.3, 'cm'),
        legend.background = element_rect(color = "black", fill = "transparent"))
p1

# Need to get the summarized data within bins
df_p <- ggplot_build(p1)$data[[1]]

pfig <- ggplot(data = df_p,
               aes(x = exp(x), y = y, z = value)) +
  scale_x_log10(expand = expansion(mult = c(0, 0))) +
  annotation_logticks(sides = "b", outside = TRUE) +
  metR::geom_contour_fill(kriging = TRUE) +
  scale_fill_viridis_c(option = "E", breaks = seq(0.5,1 ,0.25),
                       limits = c(0.4, 1),
                       name = expression(DO[sat]~"synchrony"),
                       guide = guide_colorbar(
                         direction = "horizontal")) +
  scale_y_continuous(breaks = seq(0,1,0.25), limits = c(0, 1),
                     expand = expansion(mult = c(0, 0))) +
  labs(x = "connectivity ratio (-)",
       y = "light synchrony (-)") +
  theme_bw(base_size = 10) +
  theme(panel.grid = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(0.4, 'cm'))
pfig

ggsave(plot = pfig,
       filename = file.path("results", "figures", "fig7.png"),
       dpi = 1200,
       width = 8.9,
       height = 9.2,
       units = "cm")

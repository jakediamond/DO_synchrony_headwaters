# -------------------------------------
# Author: Jake Diamond
# Purpose: Plot time series
# Date: July 10, 2020
# -------------------------------------

# Load libraries
library(lubridate)
library(readxl)
library(broom)
library(patchwork)
library(ggpubr)
library(tidyverse)

# Load DO, light, temp, and q data
df <- readRDS(file.path("data", "sensor_data_for_plotting.RDS"))
df_l <- readRDS(file.path("data", "light_data_for_plotting.RDS"))
df_q <- readRDS(file.path("data", "hourly_discharge_data_for_plotting.RDS"))

# Set plot themes ---------------------------------------------------------
themep <- theme_set(theme_bw(base_size = 10) +
                      theme(panel.grid.major.y = element_blank(), 
                            panel.grid.minor.y = element_blank(),
                            strip.placement = "outside",
                            strip.background = element_blank(),
                            strip.text.x = element_blank(),
                            legend.text = element_text(size = 8),
                            legend.title = element_text(size = 8),
                            panel.background = element_rect(fill='transparent'), 
                            plot.background = element_rect(fill='transparent', color=NA),
                            legend.key.height = unit(0.35, 'cm'),
                            legend.key.width = unit(0.35, 'cm'),
                            legend.position = "none",
                            legend.box = "horizontal",
                            legend.background = element_rect(color = "black", fill = "transparent"),
                            legend.box.background = element_rect(color = "transparent", fill = "transparent"),
                            plot.tag.position = c(0.09, 0.93),
                            axis.text.x = element_blank(),
                            axis.ticks.x = element_blank(),
                            axis.title.x = element_blank(),
                            axis.text.y = element_blank(),
                            axis.ticks.y = element_blank(),
                            plot.margin = unit(c(0, 0, 0, 0), "cm")))
# Plotting ----------------------------------------------------------------
p_fig1a <- ggplot(data = filter(df, name %in% c("DO"))) +
  geom_line(aes(x = datetime, y = value + 80 * as.numeric(stra) / 5,
                color = stra, 
                group = stra),
            size = 0.3) +
  scale_color_viridis_d(name = "Strahler order") + 
  geom_hline(aes(yintercept = 100 + 80* as.numeric(stra) / 5, 
                 color = stra),
             linetype = "dashed") +
  facet_grid(~year, scales = "free_x", 
             space = "free_x",
             labeller = label_parsed) +
  scale_x_datetime(expand = c(0,0),
                   date_breaks = "1 month") +
  labs(x = "", y = expression(DO[sat]~"(%)")) +
  guides(color=guide_legend(ncol=2)) +
  theme(legend.position = c(0.49, 0.24))

p_fig1a

p_fig1b <- ggplot(data = filter(df, name %in% c("temp")),
                  aes(x = datetime, y = value + 20 * as.numeric(stra) / 5, 
                      color = stra, 
                      group = stra)) +
  geom_line() +
  scale_color_viridis_d(name = "Strahler order") + 
  geom_hline(aes(yintercept = 15 + 20 * as.numeric(stra) / 5, color = stra), linetype = "dashed") +
  facet_grid(~year(datetime), scales = "free_x", 
             switch = "y",
             space = "free_x",
             labeller = label_parsed) +
  scale_x_datetime(expand = c(0,0),
                   date_breaks = "1 month") +
  labs(x = "", y = expression("water temp. ("*degree*"C"*")")) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

p_fig1b

p_fig1c <- ggplot() +
  geom_point(data = df_l,
             aes(x = datetime, y = light,
                 color = stra, 
                 group = stra)) +
  scale_color_viridis_d(name = "Strahler order") + 
  facet_grid(~year(datetime), scales = "free_x", space = "free_x") +
  scale_x_datetime(expand = c(0,0),
                   date_breaks = "1 month") +
  labs(x = "", y = expression("light (W "*m^{-2}*d^{-1}*")")) +
  theme(axis.text.y = element_text(),
        axis.ticks.y = element_line())
p_fig1c

# Discharge plot
p_fig1q <- ggplot() +
  geom_line(data = df_q,
            aes(x = datetime, y = value, 
                color = stra, 
                group = stra)) +
  scale_color_manual(name = "Strahler order",
                     values = c("#2b7f8c", "#5ccc64", "#f5e66b")) + 
  facet_grid(~year, scales = "free_x", 
             space = "free_x") +
  scale_x_datetime(expand = c(0,0),
                   date_breaks = "1 month",
                   date_labels = "%m/%y") +
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  labs(x = "", y = expression("Q ("*m^3~s^{-1}*")")) +
  theme(axis.text.y = element_text(),
        axis.ticks.y = element_line(),
        axis.text.x = element_text(),
        axis.ticks.x = element_line())
p_fig1q


p_fig1_all <- cowplot::plot_grid(p_fig1a, p_fig1b, p_fig1c, p_fig1q, ncol = 1, 
                                 align = "v",
                                 labels = "auto",
                                 label_fontface = "plain",
                                 label_x = 0.075, label_y = 0.975)

ggsave(plot = p_fig1_all,
       filename = file.path("results", "figures", "fig3.png"),
       dpi = 1200,
       width = 18.4,
       height = 18.4,
       units = "cm")

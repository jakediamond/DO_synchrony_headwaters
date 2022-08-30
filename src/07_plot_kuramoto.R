# 
# Purpose: To plot kuramoto among all sites and only connected sites
# Author: Jake Diamond
# Date: July 30, 2020
# 

# Load libraries
library(lubridate)
library(patchwork)
library(tidyverse)

# Load kuramoto results
df_all <- readRDS(file.path("results", "kuramoto_all_sites.RDS"))
df_con <- readRDS(file.path("results", "kuramoto_connected.RDS"))

# Load discharge data and clean for plotting
df_q <- readRDS(file.path("data", "discharge_for_plotting.RDS")) %>%
  filter(watershed %in% c("Coise", "Toranche", "Lignon"))

# General plotting info ---------------------------------------------------
# Plot layout
layout <- c(area(t = 1, l = 1, b = 3, r = 5),
            area(t = 2, l = 1, b = 3, r = 5))

theme_p <- list(
  scale_x_date(date_breaks = "1 month", date_labels = "%m/%y"),
  facet_grid(~year(date), scales = "free_x", space = "free_x"),
  theme_bw(base_size = 10),
  theme(legend.position = c(0.45, 0.23),
        strip.text = element_blank(),
        strip.background = element_blank(),
        legend.background = element_rect(color = "black", fill = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        legend.key.size = unit(0.5, "cm"),
        legend.title = element_blank(),
        legend.margin = margin(t = 0, unit='cm'),
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        axis.title.x = element_blank())
  )

# Individual Plots --------------------------------------------------------
# All sites
p_all <- df_all %>%
  mutate(r = if_else(between(datetime, ymd_h("2019110500"), ymd_h("2020030100")), 
                     NA_real_, r)) %>%
  group_by(name, date, year) %>%
  summarize(r = mean(r)) %>%
  dplyr::filter(name != "lux") %>% 
  drop_na() %>%
  ggplot(aes(x = date,
             y = r,
             color = name,
             group = interaction(name, year))) + 
  geom_point(alpha = 0.4, size = 0.9) +
  facet_grid(~year, scales = "free_x", space = "free_x") +
  stat_smooth(se = FALSE, span = 0.15) +
  scale_color_manual(values = c("black", "#ff0000", "#f2ad00"),
                     breaks = c("DO", "temp", "light"),
                     labels = c("DO", "temp.", "light")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%m/%y") +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.25)) +
  theme_p +
  labs(x = "",
       y = "synchrony (-)",
       title = "all sites",
       color = NULL)
p_all

# Plot connected sites
p_con <- df_con %>%
  mutate(r = if_else(between(date, ymd("20191105"), ymd("20200301")), 
                     NA_real_, r),
         year = year(date)) %>%
  group_by(type, date, year) %>%
  summarize(r = mean(r)) %>%
  filter(type != "lux") %>% 
  drop_na() %>%
  ggplot(aes(x = date,
             y = r,
             color= type,
             group = interaction(type, year(date)))) +
  geom_point(alpha = 0.4, size = 0.9) +
  stat_smooth(se = FALSE, span = 0.15) +
  scale_color_manual(values = c("black", "#ff0000", "#f2ad00"),
                     breaks = c("DO", "temp", "light"),
                     labels = c("DO sat.", "temp.", "light")) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.25)) +
  theme_p + 
  theme(legend.position = "none") +
  labs(x = "",
       y = "synchrony (-)",
       title = "flow-connected sites")
p_con

# Discharge plot ----------------------------------------------------------
# Get graph of specific discharge
# Only dates for the same as kuramoto
df_qp <- df_q %>%
  semi_join(distinct(df_all, date)) %>%
  filter(!between(date, ymd("20191105"), ymd("20200301")))

# Plot
p_q <- ggplot() +
  geom_line(data = df_qp,
            aes(x = date,
                y = q,
                color = watershed)) +
  scale_color_manual(values = c("#063970", "#76b5c5", "#1e81b0")) +
  scale_y_log10(position = 'right',
                breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  theme_p + 
  ylab(expression("q (mm"~d^{-1}*")")) +
  theme(rect = element_rect(fill = "transparent", color = "transparent"),
        legend.position = c(0.45, 0.27),
        axis.text.y = element_text(color = "#1f78b4"),
        axis.ticks.y = element_line(color = "#1f78b4"),
        axis.title.y = element_text(color = "#1f78b4"),
        plot.tag = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "transparent"))
p_q

# Plot everything together ------------------------------------------------
p_conq <- p_con + p_q + plot_layout(design = layout)
p_fig5 <- p_all / p_conq + plot_annotation(tag_levels = "a")
p_fig5
ggsave(plot = p_fig5,
       filename = file.path("results", "figures", "fig5.svg"),
       dpi = 1200,
       width = 13.8,
       height = 15,
       units = "cm")



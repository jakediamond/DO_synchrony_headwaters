
library(patchwork)

df_con <- readRDS(file.path("results", "kuramoto_connected.RDS"))
df_dists <- readRDS(file.path("data", "distance_ratios.RDS"))

df_all <- readRDS(file.path("results", "kuramoto_all_sites.RDS"))%>%
  filter(name %in% c("DO", "light")) %>%
  select(-nsites) %>%
  group_by(name, date) %>%
  summarize(r = mean(r, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(names_from = name, values_from = r) 

df <- filter(df_con, type %in% c("DO", "light")) %>%
  pivot_wider(names_from = type, values_from = r) %>%
  left_join(df_dists)



p <- ggplot(data = filter(df_all, year(date) ==2020, month(date) > 2),
            aes(x = light,
                y = DO)) + 
  geom_point(alpha = 0.5) +
  # scale_x_continuous(limits = c(0.85,1)) +
  # facet_wrap(~strahler) +
  # stat_summary_bin()+
  stat_smooth(method = "lm", color = "black")+
  ggpubr::stat_cor(aes(label = paste(..rr.label..)), label.y = 0.25) + 
  # # scale_color_brewer() +
  # scale_color_viridis_c(name= "connectivity\n ratio",
  #                       trans = pseudo_log_trans(sigma = 0.001, base = 10),
  #                       breaks = c(10^{-2}, 10^{-1}, 10^{0}, 10^{1}, 10^{2})) +
  # scale_color_distiller() +
  theme_bw() +
  # guides(color = guide_colorbar(title.position = "top", title.hjust = 0.5)) +
  labs(x = "light synchrony", y = expression(DO[sat]~"synchrony")) +
  theme(panel.grid = element_blank(),
        legend.direction = "horizontal",
        legend.position = c(0.80, 0.15),
        legend.key.size = unit(0.5, "cm"),
        legend.background = element_rect(color = "black", fill = "transparent"))
p


p2 <- ggplot(data = filter(df, year(date) ==2020, month(date) > 2),
            aes(x = light,
                y = DO,
                color = dist_ratio)) + 
  geom_point(alpha = 0.5) +
  # scale_x_continuous(limits = c(0.85,1)) +
  # facet_wrap(~strahler) +
  stat_summary_bin()+
  stat_smooth(color = "black")+
  # ggpubr::stat_cor(aes(label = paste(..rr.label..)), label.y = 0.25) + 
  # # scale_color_brewer() +
  scale_color_viridis_c(name= "connectivity\n ratio",
                        trans = pseudo_log_trans(sigma = 0.001, base = 10),
                        breaks = c(10^{-2}, 10^{-1}, 10^{0}, 10^{1}, 10^{2})) +
  # scale_color_distiller() +
  theme_bw() +
  guides(color = guide_colorbar(title.position = "top", title.hjust = 0.5)) +
  labs(x = "light synchrony", y = expression(DO[sat]~"synchrony")) +
  theme(panel.grid = element_blank(),
        legend.direction = "horizontal",
        legend.position = c(0.20, 0.15),
        legend.key.size = unit(0.5, "cm"),
        legend.background = element_rect(color = "black", fill = "transparent"))
p2

p3 <- p + p2 + plot_annotation(tag_levels = "a")
p3
ggsave(plot = p3,
       filename = file.path("results", "supplementary", "light_do_sync.png"),
       dpi = 1200,
       width = 18.4,
       height = 12,
       units = "cm")

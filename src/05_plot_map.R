# 
# Purpose: To plot a map of the Loire headwater sites
# Author: Jake Diamond
# Date: March 15, 2021
# 

# Load libraries
library(lubridate)
library(readxl)
library(sf)
library(stars)
library(grid)
library(tmap)
library(scales)
library(tidyverse)

# Intersecting reaches with land use --------------------------------------
# France shape
france <- read_sf(file.path("data", "GIS", "contour_France_L2E.shp"))

# Load entire loire river mainstem
loire_river <- st_read(file.path("data", "GIS", "loire_syrah.shp"))

# Read in the overall loire watershed
ws_big <- st_read(file.path("data", "GIS", "loire.shp"))

# Relief hillshade raster
relief <- read_stars(file.path("data", "GIS", "w001001.adf"))

# Read in Loire headwaters watershed files
ws <- st_read(file.path("data", "GIS", "watershed_sites_Loire_new2020.shp"))

# Read in tnet shapefile
tnet <- st_read(file.path("data", "GIS", "tnet_headwaters.shp"))
tnet <- st_set_crs(tnet, 2154)

# Load land use data
lu <- st_read(file.path("data", "GIS", "loire_headwaters_landuse.shp"))

# Read in site points
pts <- st_read(file.path("data", "GIS", "site_points_L93.shp")) %>%
  bind_rows(st_read(file.path("data", "GIS", "sites_L93.shp")) %>%
              filter(Site %in% c("Coise amont - St Symphorien",
                                 "Coise aval - Montrond")) %>%
              select(Object_ID = Site, x = Latitude, y = Longitude))

# Discharge points
pts_q <- tibble(site_code = c("K0714010", "K0704510", "K0663310", "K0773220", "K0643110"),
                watershed = c("Loise", "Toranche", "Coise", "Lignon", "Mare"),
                side = c("east", "east", "east", "west", "west"),
                pos = c("1", "2", "3", "1", "3"),
                DO = "discharge",
                y = c(6516296, 6508997, 6504317, 6515181, 6488522),
                x = c(800196, 800833, 818834, 790295, 788270)
) %>%
  st_as_sf(coords = c("x","y")) %>%
  st_set_crs(st_crs(pts)) %>%
  mutate(side = fct_relevel(side, "west", "east"))

# ws names
ws_names <- tibble(name = c("c) Loise", "d) Toranche", "e) Coise", "a) Lignon", "b) Mare"),
                   side = c("east", "east", "east", "west", "west"),
                   pos = c("1", "2", "3", "1", "3"),
                   y = c(6527400, 6515000, 6506700, 6527400, 6486000),
                   x = c(811300, 801100, 804500, 778500, 776000)) %>%
  st_as_sf(coords = c("x","y")) %>%
  st_set_crs(st_crs(pts)) %>%
  mutate(side = fct_relevel(side, "west", "east"))

# Read in la loire cut to the headwaters
loire_hw <- st_read(file.path("data", "GIS", "tnet_out.shp"))

# Read in watershed info
ws_meta <- read_excel(file.path("data", "metadata", "sensor_metadata.xlsx"),
                      sheet = 7) %>%
  mutate(side = case_when(watershed == "Mare" ~ "west",
                          watershed == "Lignon" ~ "west",
                          watershed == "Loise" ~ "east",
                          watershed == "Toranche" ~ "east",
                          watershed == "Coise" ~ "east",
                          TRUE ~ "watershed"),
         pos = case_when(watershed == "Mare" ~ "3",
                         watershed == "Lignon" ~ "1",
                         watershed == "Loise" ~ "1",
                         watershed == "Toranche" ~ "2",
                         watershed == "Coise" ~ "3",
                         TRUE ~ "watershed")) %>%
  mutate(side = fct_relevel(side, "west", "east"))

# Join that info to the ws file
ws <- ws %>%
  arrange(name_site) %>%
  bind_cols(arrange(drop_na(ws_meta, name_site), name_site) %>%
              filter(site_code != "fon10") %>%
              select(-name_site))

# Add watershed data to pts
pts <- pts %>%
  arrange(as.character(Object_ID)) %>%
  bind_cols(mutate(ws_meta, 
                   Object_ID = if_else(is.na(Object_ID),name_site,Object_ID)) %>%
              arrange(Object_ID) %>%
              select(-Object_ID)) %>%
  mutate(DO = if_else(type == "chimie seulement", NA_character_,
                      if_else(type == "OD et T 2020",
                              "2020",
                              if_else(type == "OD et T 2019-2020",
                                      "2019 & 2020",
                                      "2019")))) %>%
  filter(!is.na(DO))

# Intersect landuse and veg to watersheds
lu_ws <- st_intersection(ws, st_make_valid(lu)) %>%
  st_collection_extract("POLYGON") # some of the intersections produce "GEOMETRY" type, don't want

# Meta data for landuse
meta_lu <- read_xlsx(file.path("data", "metadata", "clc_meta.xlsx"))
breaks <- arrange(distinct(meta_lu, plot_name, plot_code, .keep_all = TRUE), plot_code)

# Plotting ----------------------------------------------------------------
# Overall plots of just points
# Want to get the biggest watersheds to prevent over-plotting
ws_large <- ws %>%
  group_by(watershed) %>%
  filter(surf_km2 == max(surf_km2)) %>%
  ungroup() %>%
  mutate(name = c("e", "a", "c", "b", "d"))

# Intersect tnet reaches to the watersheds
ws_int <- st_intersection(ws_large, tnet)

# Land use at the watershed scale
lu_ws_large <- lu_ws %>%
  group_by(watershed) %>%
  filter(surf_km2 == max(surf_km2)) %>%
  ungroup() %>%
  left_join(meta_lu)

tmap_mode("view")

# Plot watersheds with points
p_lu_pts <- tm_shape(ws_large) + tm_borders(col = "black") +
  tm_scale_bar(position = c("RIGHT", "BOTTOM"), text.size = 1.2) +
  tm_shape(ws_int) +  tm_lines(lwd = "OSTRAHL", col = "blue",
                               legend.lwd.show = FALSE,
                               scale = 1.6,
                               alpha = 0.7) +
  tm_shape(pts) +
  tm_symbols(size = 0.001,
             col = "DO",
             palette = c("white", "black", "grey"),
             alpha = 0.7,
             border.col = "black",
             border.lwd = 1.5) +
  tm_layout(frame = FALSE, frame.lwd = NA,
            panel.label.bg.color = NA,
            legend.show = FALSE,
            panel.show = FALSE)
p_lu_pts

# Bounding box
hw_bb = st_as_sfc(st_bbox(ws))
hw_bb2 <- st_transform(hw_bb, st_crs(relief))
ws_rlf2 <- st_crop(relief, hw_bb2)
plot(ws_rlf2)

alt <- raster::raster(file.path("data", "GIS", "w001001.adf"))
slope <- raster::terrain(alt, opt = 'slope')
aspect <- raster::terrain(alt, opt = 'aspect')
hill <- raster::hillShade(slope, aspect, 30, 90)

hw_bb <- st_as_sfc(st_bbox(ws))
hw_bb2 <- st_transform(hw_bb, st_crs(hill))

# Get an inset map
# Inset map of just rivers
region_map <-tm_shape(hill, bbox = hw_bb2, raster.downsample = FALSE) +
  tm_raster(palette = gray(0:100 / 100), n = 100, legend.show = FALSE) +
  tm_shape(ws_large) + tm_borders(col = "black", lwd = 2) +
  tm_text(text = "name") +
  tm_grid(labels.inside.frame = FALSE, 
          lines = FALSE,
          projection = "+proj=longlat",
          labels.format = list(fun=function(x) {paste0(x,intToUtf8(176))} ) )
region_map

# France map
loire_france_tm <- 
  tm_graticules() +
  tm_shape(france) +
  tm_borders() +
  tm_shape(ws_big) + tm_borders(col = "#3B9AB2", lwd = 3) +
  tm_shape(loire_river) + tm_lines(col = "blue", lwd = 3) +
  tm_shape(hw_bb) + tm_borders(col = "red", lwd = 3)
loire_france_tm

# Save map
tmap_save(p_lu_pts, filename = file.path("results", "figures", "fig2.svg"),
          dpi = 1200,
          width = 18.4, height = 20,
          units = "cm",
          insets_tm = list(region_map,
                           loire_france_tm),
          insets_vp = list(viewport(0.487, 0.415, width = 0.33, height = 0.45,
                                    gp = gpar(col = "red", fill = "red")),
                           viewport(0.47, 0.575, width = 0.37*0.7, height = 0.55*0.7))
)

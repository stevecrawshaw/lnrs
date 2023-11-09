pacman::p_load(tidyverse,
               rgbif,
               httr2,
               glue,
               janitor,
               sf,
               RPostgreSQL)
source("../airquality_GIT/gg_themes.R")

# download from gbif
fungi_tbl <- read_tsv('data/fungi_woe_gbif.csv')
# just to check geo distribution
woe <- st_read(dsn = "C:\\Users\\steve.crawshaw\\OneDrive - West Of England Combined Authority\\Documents\\qgis\\gis_files\\WOE MM CLIPPED.TAB") %>% 
  st_transform(crs = 27700)

# make an SFC of the species post 2020
fungi_sf <- fungi_tbl %>% 
  filter(eventDate >= as.Date("2020-01-01")) %>% 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) %>% 
  st_transform(crs = 27700)

#write out to shapefile
fungi_sf %>% 
  st_write('data/fungi2020.shp')

# quick analysis of who collects
fungi_sf %>% 
  count(institutionCode) %>% 
  st_drop_geometry()

fungi_obs_count_plot <- fungi_tbl %>% 
  filter(year >= 2000) %>% 
  group_by(year, institutionCode) %>% 
  summarise(count = n()) %>%
  filter(count > 10, institutionCode != "NA") %>% 
  ggplot(aes(x = year, y = count, colour = institutionCode)) +
  geom_line(linewidth = 2) +
  scale_x_continuous(breaks = seq.int(2000, 2023, by = 2)) +
  theme_web_classic() +
  labs(title = "Fungi: Species Observations by Institution",
       subtitle = "Unassigned institutions and low counts were removed",
       caption = "GBIF.org (21 May 2023) GBIF Occurrence Download https://doi.org/10.15468/dl.n2ee2k",
       colour = "Institution",
       x = "Year",
       y = "Count") +
  theme(plot.subtitle = element_text(vjust = 6),
        axis.text.x = element_text(size = 10))
  
fungi_obs_count_plot

fungi_obs_count_plot %>% 
  ggsave('plots/fungi_obs_count_plot.png', ., device = 'png')

ggplot() +
  geom_sf(data = woe) +
  geom_sf(data = fungi_sf)


 

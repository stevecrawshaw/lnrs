pacman::p_load(fastverse,
               tidyverse,
               glue,
               janitor,
               sf)

woe <- st_read('data/weca_boundary.geojson') %>% 
  st_transform(crs = 4326)

pre_2016 <- read_csv('data/predicts/database.csv')

pre_2016 %>% 
  head(10) %>% 
  glimpse()
pre_2022 <- read_rds('data/predicts/database2ndrelease.rds')

pre_all <- bind_rows(pre_2016, pre_2022)
pre_all %>% write_rds('data/predicts/predict_all_to_2022.rds')

pre_all <- read_rds('data/predicts/predict_all_to_2022.rds')

pre_uk <- pre_all %>% 
  filter(Country == "United Kingdom")


# rm(pre_2016, pre_2022, pre_all)


uk_pre_sf <- pre_uk %>% 
  st_as_sf(coords = c('Longitude', 'Latitude'), crs = 4326)

uk_pre_sf %>%  glimpse()

woe_pre <- uk_pre_sf %>% 
  st_intersection(woe)


ggplot() +
  geom_sf(data = woe) +
  geom_sf(data = woe_pre,
          aes(color = Indication),
          size = 3)

woe_pre %>% 
  group_by(Study_name, Taxon_name_entered, Sample_midpoint) %>% 
  summarise(count = n()) %>% 
  view()


plot(woe_pre %>% select(Taxon_name_entered))
  
  
  

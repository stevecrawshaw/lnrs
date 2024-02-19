pacman::p_load(tidyverse,
               sf,
               glue,
               janitor,
               fs,
               rlist)


priority_raw_sf <- st_read("data/scratch/valid_geom.geojson")

g <- priority_raw_sf %>% 
  st_make_valid()
  
  g %>% 
    st_write("data/scratch/see_if_valid.geojson")
  
  filter(id == 34) %>% 
  st_geometry()


plot(priority_raw_sf)


priority_raw_sf %>% 
  st_buffer(0)

just_valid <- priority_raw_sf %>% 
  filter(st_is_valid(.)) 


base::setdiff(priority_raw_sf, just_valid)

plot(just_valid)  
  

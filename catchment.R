pacman::p_load(tidyverse,
               glue,
               janitor,
               sf)


catch_sf <- st_read('data/catchment_hydro/data/CAMELS_GB_catchment_boundaries/CAMELS_GB_catchment_boundaries.shp')

woe <- st_read('data/weca_boundary.geojson')


ggplot() +
  geom_sf(data = woe)

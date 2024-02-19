pacman::p_load(tidyverse,
               sf,
               glue,
               janitor,
               fs,
               rlist)


yr1_tbl <- st_read("data/Avon_TfC_Y1.gpkg")
yr2_tbl <- st_read("data/Avon_TfC_Y2.gpkg")
yr3_tbl <- st_read("data/Avon_TfC_Y3.gpkg")

yr3_tbl %>% 
  st_transform(crs = 4326) %>% 
  st_write("data/year_3_trees.geojson")


y1_trees <- yr1_tbl %>% 
  st_drop_geometry() %>% 
  transmute(Site_ID, Trees.Planted = n_trees, yr = 2021)

y2_trees <- yr2_tbl %>% 
  st_drop_geometry() %>% 
  transmute(Site_ID, Trees.Planted, yr = 2022)

y3_trees <- yr3_tbl %>% 
  st_drop_geometry() %>% 
  transmute(Site_ID, Trees.Planted, yr = 2023)

collated = bind_rows(y1_trees, y2_trees, y3_trees)

plantings_tbl <- collated %>% 
  group_by(yr, Site_ID) %>% 
  summarise(ntrees = mean(Trees.Planted, na.rm = TRUE)) %>% 
  summarise(trees_per_year = sum(ntrees) %>% as.integer())

sum(plantings_tbl$trees_per_year)
  

plantings_tbl %>% 
  write_csv("data/foa_trees_for_climate_year.csv")

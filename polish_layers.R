pacman::p_load(tidyverse,
               sf,
               glue,
               janitor,
               fs)

scratch_path <- fs::path("data", "scratch")

layers_list <- fs::dir_ls(scratch_path, glob = "*.geojson") %>% 
  map(~st_read(.))
names(layers_list)

fs::path_file(layers_list %>% names()) %>% path_ext_remove() %>% make_clean_names()

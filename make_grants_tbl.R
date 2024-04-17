pacman::p_load(tidyverse,
               glue,
               janitor
               )

area_measures_long_tbl <- readr::read_delim("https://westofenglandca.opendatasoft.com/api/explore/v2.1/catalog/datasets/area-measures-long-tbl/exports/csv?lang=en&timezone=Europe%2FLondon&use_labels=false&delimiter=%2C", delim = ",")

# make a table that can be filtered by measures showing all the grants available for that measure

grants_links_long_tbl <- area_measures_long_tbl %>% 
  select(measure,
         area_measure_id, 
         # link_to_further_guidance,
         # scheme, 
         grant_link) %>% 
  # separate_longer_delim(cols = scheme, delim = "\n") %>%
  separate_longer_delim(cols = grant_link, delim = "\n") %>% 
  distinct()

grants_links_long_tbl %>% glimpse()


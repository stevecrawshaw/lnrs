pacman::p_load(fastverse,
               tidyverse,
               glue,
               janitor)

raw_ods_tbl <- fread("https://westofenglandca.opendatasoft.com/api/explore/v2.1/catalog/datasets/area-measures-new-tbl/exports/csv?lang=en&timezone=Europe%2FLondon&use_labels=false&delimiter=%2C")

raw_ods_tbl %>% 
  write_csv("data/area-measures-new-tbl_orig.csv", na = "")

raw_ods_tbl %>% 
  distinct(land_type) 
raw_ods_tbl %>% 
  as_tibble() %>% 
  mutate(stakeholder = str_replace_all(stakeholder,
                                       "&", "and") %>% 
           str_to_sentence() %>% 
           str_replace("los", "landholders")) %>%
  write_csv("data/area-measures-new-tbl.csv", na = "")
  
  

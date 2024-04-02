pacman::p_load(tidyverse,
               glue,
               janitor,
               fs)

tbl_list <- read_rds("portal_tbl_list.rds")

names(tbl_list) <- str_replace_all(names(tbl_list), pattern = "-", replacement = "_")

list2env(tbl_list, .GlobalEnv)


species_lookup <- species_area_lookup_tbl %>% 
  left_join(species_priority_lookup_tbl,
            by = join_by(species_id == species_id),
            relationship = "many-to-many") %>% 
  select(-starts_with("id.")) 

grants_tbl %>% 
  left_join(priority_measures_grants_lookup_tbl,
            by = join_by(grant_id == grant_id)) %>% 
  left_join(areas_measures_grants_lookup_tbl,
            by = join_by(grant_id == grant_id),
            relationship = "many-to-many") %>% 
  view()


comb_tbl <- areas_tbl %>% 
  left_join(priorities_areas_lookup_tbl, by = join_by(area_id == area_id)) %>% 
  right_join(priorities_tbl, by = join_by(priority_id == priority_id)) %>% 
  right_join(species_lookup, by = join_by(area_id == area_id,
                                          priority_id == priority_id)) %>% 
  left_join(species_tbl %>% select(species_id, common_name), by = join_by(species_id == species_id)) %>% 
  left_join(priorities_measures_lookup_tbl,
            by = join_by(priority_id == priority_id),
            relationship = "many-to-many") %>% 
  left_join(priority_measures_tbl, 
            by = join_by(priority_measure_id == priority_measure_id)) %>% 
  right_join(priority_measures_grants_lookup_tbl, by = join_by(priority_measure_id == priority_measure_id),
             relationship = "many-to-many") %>% 
  right_join(grants_tbl, by = join_by(grant_id == grant_id)) %>% 
  left_join(areas_measures_grants_lookup_tbl,
            by = join_by(grant_id == grant_id),
            relationship = "many-to-many") %>% 
  left_join(area_measures_tbl, by = join_by(area_measure_id == area_measure_id)) %>% 
  left_join(area_funding_schemes_tbl, by = join_by(area_id == area_id),
            relationship = "many-to-many")

comb_tbl %>% nrow()


comb_tbl %>% 
  select(!starts_with("id.")) %>% 
  #distinct(area_id, priority_id, priority_measure_id, area_measure_id, grant_id, species_id, funding_schemes) %>% 
  filter(!is.na(area_name),
         area_id == 1) %>% 
  group_by(priority_id, priority_measure_id, area_measure_id) %>% 
  summarise(spec = paste(common_name, collapse = "; "),
            grant = paste(grant_id, collapse = "; ")) %>% 
  
  view()
  glimpse()





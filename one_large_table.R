pacman::p_load(tidyverse,
               glue,
               janitor,
               fs)
# extract the source data which are tables in duckdb 

tbl_list <- read_rds("portal_tbl_list.rds")

names(tbl_list) <- str_replace_all(names(tbl_list), pattern = "-", replacement = "_")

list2env(tbl_list, .GlobalEnv)

# make a table which relates areas, priorities, measures and grants

areas_priorities_grants_tbl <- areas_tbl %>% 
  left_join(priorities_areas_measures_lookup_tbl, by = join_by(area_id == area_id)) %>% 
  select(-id) %>%
  left_join(area_measures_tbl, by = join_by(area_measure_id == area_measure_id)) %>% 
  left_join(priorities_tbl, by = join_by(priority_id == priority_id)) %>%
  left_join(priorities_measures_lookup_tbl,
            by = join_by(priority_id == priority_id),
            relationship = "many-to-many") %>%
  left_join(priority_measures_tbl,
            by = join_by(priority_measure_id == priority_measure_id),
            suffix = c("_area", "_priority")) %>%
  left_join(priority_measures_grants_lookup_tbl,
            by = join_by(priority_measure_id == priority_measure_id),
            relationship = "many-to-many") %>% 
  left_join(areas_measures_grants_lookup_tbl, 
            by = join_by(area_measure_id == area_measure_id),
            relationship = "many-to-many", suffix = c("_p_measures", "_a_measures")) %>%
  pivot_longer(cols = c(grant_id_p_measures, grant_id_a_measures),
               names_to = "grant_area_or_priority",
               values_to = "grant_id") %>%
  mutate(grant_area_or_priority = str_remove_all(grant_area_or_priority, "^grant_id_|_measures$")) %>%
    left_join(grants_tbl, by = join_by(grant_id == grant_id)) %>% 
  select(-starts_with("id")) 

# make a table which relates species, areas and priorities

species_area_priority_tbl <- 
areas_priorities_grants_tbl %>%
  select(area_id, priority_id) %>% 
  left_join(species_area_lookup_tbl,
            by = join_by(area_id == area_id),
            relationship = "many-to-many") %>% 
  left_join(species_priority_lookup_tbl,
            by = join_by(priority_id == priority_id),
            relationship = "many-to-many",
            suffix = c("_area", "_priority")) %>%
  pivot_longer(cols = c(species_id_area, species_id_priority),
               names_to = "species_area_priority",
               values_to = "species_id") %>%
  distinct(area_id, priority_id, species_id) %>% 
  left_join(species_tbl, by = join_by(species_id == species_id))


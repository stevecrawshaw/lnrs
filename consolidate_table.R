pacman::p_load(tidyverse,
               glue,
               janitor,
               fs)

tbl_list <- read_rds("portal_tbl_list.rds")

names(tbl_list) <- str_replace_all(names(tbl_list), pattern = "-", replacement = "_")

list2env(tbl_list, .GlobalEnv)

clean_text <- function(x){ 
  x %>% 
    str_remove_all("\nNA") %>% 
    str_split("\n") %>% 
    unlist() %>% 
    unique() %>% 
    paste(collapse = "\n") 
}

species_lookup <- species_area_lookup_tbl %>% 
  left_join(species_priority_lookup_tbl,
            by = join_by(species_id == species_id),
            relationship = "many-to-many") %>% 
  select(-starts_with("id.")) 

species_lookup %>% glimpse()

grants_measures_tbl <- grants_tbl %>% 
  left_join(priority_measures_grants_lookup_tbl,
            by = join_by(grant_id == grant_id)) %>% 
  left_join(areas_measures_grants_lookup_tbl,
            by = join_by(grant_id == grant_id),
            relationship = "many-to-many") %>% 
  left_join(priority_measures_tbl,
            by = join_by(priority_measure_id == priority_measure_id)) %>% 
  left_join(area_measures_tbl,
            by = join_by(area_measure_id == area_measure_id),
            suffix = c("_priority","_area")) %>% 
  left_join(priorities_measures_lookup_tbl,
            by = join_by(priority_measure_id == priority_measure_id),
            relationship = "many-to-many") %>% 
  left_join(priorities_areas_measures_lookup_tbl,
            by = join_by(area_measure_id == area_measure_id),
            relationship = "many-to-many",
            suffix = c("_priority_measures", "_area_measures")) %>%
  select(-starts_with("id")) %>% 
  mutate(priority_id = glue("{priority_id_priority_measures};{priority_id_area_measures}")) %>% 
  separate_longer_delim(priority_id, delim = ";") %>% 
  filter(!is.na(priority_id) & !is.na(area_id)) %>% 
  mutate(priority_id = na_if(priority_id, "NA") %>% as.integer()) %>% 
  arrange(area_id, priority_id)


priorities_areas_tbl <- priorities_areas_lookup_tbl %>% 
  left_join(areas_tbl, by = join_by(area_id == area_id)) %>% 
  left_join(priorities_tbl, by = join_by(priority_id == priority_id)) %>% 
  select(-id) %>% 
  arrange(area_id, priority_id)

priorities_areas_tbl %>% view()

consolidated_tbl <- priorities_areas_tbl %>% 
  left_join(grants_measures_tbl, by = join_by(
    area_id == area_id,
    priority_id == priority_id
  )) %>% 
  arrange(area_id, priority_id)

consolidated_tbl %>% view()

grpd_tbl <- consolidated_tbl %>% 
  group_by(area_id, area_name, area_description, area_link,
           priority_id, biodiversity_priority, simplified_biodiversity_priority) %>% 
  summarise(across(.cols = c(grant_name, measure_priority, measure_area),
                   ~paste(.x, collapse = "\n"))) %>% 
  mutate(across(.cols = c(grant_name, measure_priority, measure_area),
                clean_text))


grpd_tbl %>% view()

grpd_tbl %>% ungroup() %>% 
  select(measure_priority_clean) %>% head()


d <- grpd_tbl$measure_priority[3]





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





pacman::p_load(tidyverse,
               glue,
               janitor,
               fs)
# extract the source data which are tables in duckdb 

tbl_list <- read_rds("portal_tbl_list.rds")

names(tbl_list) <- str_replace_all(names(tbl_list), pattern = "-", replacement = "_")

list2env(tbl_list, .GlobalEnv)

# formatting functions ----
format_nice <- function(str){
  str_replace_all(str, pattern = "_", replacement = " ") %>% 
    str_to_sentence()
}

make_kv <- function(dotx){
  if_else(is.na(dotx),
          str_c(cur_column() %>% format_nice(),
                ": NA"),
          str_c(cur_column() %>% format_nice(), ": ",
                dotx))
}

# prep measures tbls for better formatting ----

area_measures_single_tbl <- area_measures_tbl %>% 
  mutate(across(-area_measure_id, ~make_kv(.x)
                )) %>%
    unite(col = "area_measures",
          sep = "\n",
          -area_measure_id) 

priority_measures_single_tbl <- priority_measures_tbl %>% 
  mutate(across(-priority_measure_id, ~make_kv(.x)
                )) %>%
    unite(col = "priority_measures",
          sep = "\n",
          -priority_measure_id)

grants_single_tbl <- grants_tbl %>% 
  select(-id) %>% 
  relocate(url, .after = everything()) %>% 
  rename(link = url) %>% 
  mutate(across(-c(grant_id), ~make_kv(.x)
                )) %>%
    unite(col = "grants",
          sep = "\n",
          -grant_id)

# make a table which relates species, areas and priorities ----

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
  left_join(species_tbl %>% 
              select(species_id, common_name),
            by = join_by(species_id == species_id))

# make a table which relates areas, priorities, measures and grants

areas_priorities_grants_tbl <- areas_tbl %>% 
  left_join(priorities_areas_lookup_tbl,
            by = join_by(area_id == area_id)) %>% 
  left_join(priorities_areas_measures_lookup_tbl,
            by = join_by(area_id == area_id,
                         priority_id == priority_id)) %>% 
  left_join(area_measures_single_tbl, by = join_by(area_measure_id == area_measure_id)) %>% 
  left_join(priorities_tbl, by = join_by(priority_id == priority_id)) %>%
  left_join(priorities_measures_lookup_tbl,
            by = join_by(priority_id == priority_id),
            relationship = "many-to-many") %>%
  left_join(priority_measures_single_tbl,
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
    left_join(grants_single_tbl, by = join_by(grant_id == grant_id)) %>% 
  left_join(species_area_priority_tbl,
            by = join_by(area_id == area_id,
                         priority_id == priority_id),
            relationship = "many-to-many") %>%
  select(-starts_with("id")) %>% 
  distinct()

consolidated_tbl <- areas_priorities_grants_tbl %>% 
  group_by(area_id, area_name, area_description, area_link,
           priority_id, theme, biodiversity_priority, simplified_biodiversity_priority) %>% 
  summarise(across(where(is.character), ~str_c(unique(.x[!is.na(.x)]),
                                        collapse = "\n\n")),
            across(where(is.numeric), ~str_c(unique(.x[!is.na(.x)]),
                                        collapse = ": ")),
            .groups = "drop") 









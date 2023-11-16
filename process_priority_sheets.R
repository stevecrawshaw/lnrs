pacman::p_load(tidyverse,
               glue,
               janitor,
               fs,
               readxl)
path = "data/Local Priorities Master for portal_updated format.xlsx"
sheets_vec <-
  readxl::excel_sheets(path)

make_list_from_sheets <- function(
    path = "data/Local Priorities Master for portal_updated format.xlsx",
    sheets_vec) {
  sheets_vec %>%
    map(~ read_xlsx(path, .x)) %>%
    set_names(make_clean_names(sheets_vec)) %>%
    map(.f = clean_names)
}

sheets_list <- make_list_from_sheets(path, sheets_vec)

clean_text <- function(input_string) {
  # Step 1: Replace non-space-padded hyphens with a placeholder
  # Use a unique placeholder that is unlikely to be in the text
  placeholder <- "UNIQUE_PLACEHOLDER"
  output_string <- input_string %>%
    str_replace_all("(?<!\\s)-(?!\\s)", placeholder) %>%
    # Step 2: Replace commas, double spaces, and semicolons with spaces
    str_replace_all("[,;]|\\s{2,}", " ") %>%
    # Step 3: Replace the unique placeholder with a hyphen with space padding
    str_replace_all(placeholder, " - ")
  return(output_string)
}

parse_areas_tbl <- function(sheets_list) {
  # import and process the priority areas
  sheets_list %>%
    pluck("areas_for_description") %>%
    rename(area_id = identifier,
           area_name = title,
           area_link = `links_to_further_info_guidance`) %>%
    mutate(
      area_id = as.integer(area_id),
      area_name = map_chr(area_name, ~ clean_text(.x)),
      area_description = "Lorem Ipsum"
    )
}

parse_priorities_tbl <- function(sheets_list, areas_count = 47) {
  # read and process the priorities table. 
  # amend area_count var if new areas are added
  
  priorities_raw_tbl <-
    sheets_list %>% pluck("statement_of_bd_priorities")
  
  nms <- c("theme",
           "priority_id",
           "biodiversity_priority",
           1:areas_count)
  
    priorities_raw_tbl %>%
    filter(x1 != "Theme") %>%
    set_names(nms) %>%
    mutate(across(
      .cols = 4:last_col(),
      ~ if_else(.x == "x", cur_column(),
                NA_character_)
    )) %>%
    pivot_longer(cols = 4:last_col(), values_to = "area_id") %>%
    select(-name) %>%
    filter(!is.na(area_id)) %>%
    mutate(across(.cols = ends_with("_id"), as.integer))
}


# parse_measures_tbl <- function(sheets_list){
 # consolidate measures by priority and measures by area sheets
  # extend, separating out codes
measures_priority_tbl <- sheets_list %>% pluck("measures_by_priority") 
measures_area_tbl <- sheets_list %>% pluck("measures_by_area") 

measures_tbl <- bind_rows(measures_area_tbl, measures_priority_tbl)

measures_tbl %>% 
  mutate(measure = coalesce(recommended_measures, 
                            recommended_measure)
         ) %>% 
  rownames_to_column(var = "measure_id") %>% 
  mutate(across(.cols = c(level_of_ambition, land_type),
                .fn = ~if_else(.x == "N/A",
                               NA_character_,
                               .x))) %>% 
  separate_longer_delim(cols = c(stakeholder,
                                 countryside_stewardship,
                                 sfi),
                        delim = "; ") %>% 
  pivot_longer(starts_with("associated_priority"),
               values_to = "priority_id") %>% #glimpse()
  filter(!is.na(priority_id)) %>% 
  select(-name,
         -starts_with("recommended")) %>%
  # line below fails if done above!
  separate_longer_delim(associated_area_codes, delim = "; ") %>% 
  select(measure_id, measure, ambition = level_of_ambition, land_type,
         stakeholder_type = stakeholder, area_id = associated_area_codes,
         priority_id, guidance = link_to_further_guidance, everything()) %>%
  view()
# }





priorities_tbl <- priorities_base_tbl %>%
  distinct(priority_id, priority_description)

# lookup between priorities and areas
priority_area_tbl <- priorities_base_tbl %>%
  select(-priority_description)

rec_area_tbl <- recommendations_area_raw_tbl %>%
  clean_names() %>%
  separate_rows(associated_area_codes, sep = "; ") %>%
  mutate(associated_area_codes = as.integer(associated_area_codes)) %>%
  glimpse()

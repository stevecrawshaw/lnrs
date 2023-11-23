pacman::p_load(tidyverse,
               glue,
               janitor,
               fs,
               readxl,
               writexl,
               rgbif)
# Source Data ----
path = "data/Local Priorities Master for portal_updated format.xlsx"
sheets_vec <-
  readxl::excel_sheets(path)
# Utility Functions ----
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

make_list_from_sheets <- function(
    path = "data/Local Priorities Master for portal_updated format.xlsx",
    sheets_vec) {
  # read all the sheets and store in a list with each element a sheet
  sheets_vec %>%
    map(~ read_xlsx(path, .x)) %>%
    set_names(make_clean_names(sheets_vec)) %>%
    map(.f = clean_names)
}
# Process source data from spreadsheet ----
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

parse_priorities_tbl <- function(sheets_list, areas_count = 50) {
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

parse_measures_tbl <- function(sheets_list){
 # consolidate measures by priority and measures by area sheets
  # extend, separating out codes
  
  # need to sort out the grants - pivoting longer etc.
  
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
                               .x)),
         # remove text descriptions and add a PH1 placeholder
         # designation pending assignation of correct codes
         countryside_stewardship = if_else(!str_starts(countryside_stewardship,
                                 "\\b[A-Z]{2}\\d{1,2}\\b"),
                      "PH1", countryside_stewardship) %>%
           str_extract_all("\\b[A-Z]{2}\\d{1,2};?\\b") %>% 
           map_chr( ~paste0(.x, collapse = "; ")),
         #paste0(..., collapse) coerces NA to "NA"!
         countryside_stewardship = if_else(countryside_stewardship == "NA",
                                           NA_character_,
                                           countryside_stewardship)) %>% 
  separate_longer_delim(cols = c(stakeholder,
                                 countryside_stewardship,
                                 sfi),
                        delim = "; ") %>% 
  pivot_longer(starts_with("associated_priority"),
               values_to = "priority_id") %>% 
  filter(!is.na(priority_id)) %>% 
  select(-name,
         -starts_with("recommended")) %>%
  # line below fails if done above!
  
  separate_longer_delim(associated_area_codes,
                        delim = "; ") %>% 
  pivot_longer(cols = c(countryside_stewardship, sfi, woodland, other),
                 names_to = "grant_scheme",
                 values_to = "link_or_code") %>% 
  filter(!is.na(link_or_code)) %>% 
  select(measure_id, measure, ambition = level_of_ambition, land_type,
         stakeholder_type = stakeholder, area_id = associated_area_codes,
         priority_id, guidance = link_to_further_guidance, everything()) %>% 
  mutate(across(.cols = ends_with("_id"), as.integer))
}

save_tbls <- function(tbl_list){
  # take a named list and write csv's for the portal 
  # and an excel file for introspection
  # csv - semicolon delims
  nms = paste0("data/", names(tbl_list), ".csv")
  walk2(tbl_list, nms, ~write_csv2(.x, .y, na = ""))
  # excel for viewing
  write_xlsx(tbl_list, path = "data/main_sheets.xlsx")
  
}

# Use chat GPT to get the Linnaean names https://chat.openai.com/share/086bf029-f2a7-409a-b60c-a5964015df21

make_priority_species_tbl <- function(sheets_list){
sheets_list %>% 
  pluck("priority_species") %>% 
  rownames_to_column(var = "species_id") %>% 
  mutate(linnaean_name = str_extract(linnaean,
                                     "\\*([A-Z][a-z]+\\s[a-z]+)") %>% 
           str_replace_all("\\*", ""),
         linnaean = NULL,
         species_id = as.integer(species_id))
}

get_gbif_tbl <- function(priority_species_tbl){
# get the gbif definitive species data
priority_species_tbl %>% 
  pull(linnaean_name) %>% 
  name_backbone_checklist() %>% 
  select(-rank, -confidence, -matchType, acceptedUsageKey) %>% 
  mutate(gbif_species_url = glue("https://www.gbif.org/species/{usageKey}"))
}

make_species_area_priority_lookup_tbl <- function(priority_species_tbl){
priority_species_tbl %>% 
  separate_longer_delim(cols = relevant_areas,
                        delim = "; ") %>%
  separate_longer_delim(cols = relevant_priorities,
                        delim = "; ") %>%
  rename(area_id = relevant_areas,
         priority_id = relevant_priorities) %>%
  mutate(across(ends_with("_id"), as.integer)) %>%
  distinct(species_id, area_id, priority_id) 
}

make_species_tbl <- function(priority_species_tbl, gbif_tbl){
priority_species_tbl %>% 
  select(-species, -relevant_priorities, -relevant_areas, -link_to_further_guidance, -linnaean_name) %>% 
  inner_join(gbif_tbl, by = join_by(species_id == verbatim_index))
}


# Test functions and generate data ----

sheets_list <- make_list_from_sheets(path, sheets_vec)
priority_species_tbl <- make_priority_species_tbl(sheets_list)
gbif_tbl <- get_gbif_tbl(priority_species_tbl)
species_tbl <- priority_species_tbl %>% 
  make_species_tbl(gbif_tbl)
species_area_priority_lookup_tbl <- priority_species_tbl %>% 
  make_species_area_priority_lookup_tbl()
areas_tbl <- parse_areas_tbl(sheets_list)

priorities_tbl <- parse_priorities_tbl(sheets_list, areas_count = 50) %>% 
  distinct(theme, priority_id, biodiversity_priority)

measures_tbl <- parse_measures_tbl(sheets_list) %>% 
  distinct(measure_id, measure, ambition, land_type)
priority_area_lookup_tbl <- parse_priorities_tbl(sheets_list) %>% 
  distinct(area_id, priority_id)
measures_priority_area_lookup_tbl <- parse_measures_tbl(sheets_list) %>% 
  distinct(measure_id, area_id, priority_id)

# Write Data ----

tbl_list <- list(
  areas = areas_tbl, 
  priorities = priorities_tbl, 
  measures = measures_tbl,
  species = species_tbl,
  priority_area_lookup = priority_area_lookup_tbl,
  measures_priority_area_lookup = measures_priority_area_lookup_tbl,
  species_area_priority_lookup = species_area_priority_lookup_tbl
                 )

save_tbls(tbl_list)



pacman::p_load(tidyverse,
               glue,
               janitor,
               fs,
               readxl)

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



path <- 'data/Local Priorities Master for portal_updated format.xlsx'

sheets_vec <-
  readxl::excel_sheets(path) 

areas <- sheets_vec[1]
priorities <- sheets_vec[2]
recommendations <- sheets_vec[3]

areas_raw_tbl <- read_xlsx(path, sheet = areas)
priorities_raw_tbl <- read_xlsx(path, sheet = priorities)
recommendations_raw_tbl <- read_xlsx(path, sheet = recommendations)


areas_tbl <- areas_raw_tbl %>% 
  rename(area_id = Identifier,
         area_name = Title,
         area_link = `Links to further Info`) %>% 
mutate(area_id = as.integer(area_id),
       area_name = map_chr(area_name, ~clean_text(.x)))

glimpse(areas_tbl)

priorities_base_tbl <- priorities_raw_tbl %>% 
  rename( priority_id = ...1,
          priority_description = Identifier) %>% 
  filter(priority_id != "Code") %>% 
  select(-...48) %>% 
  mutate(priority_id = priority_id %>% 
           as.double() %>% 
           `*`(10) %>% 
           round() %>% 
           as.integer()
         ) %>% 
  mutate(across(.cols = `1`:`45`,
                ~if_else(.x == "x", cur_column(),
                         NA_character_))) %>% 
  pivot_longer(cols = `1`:`45`) %>% 
  select(starts_with("priority"),
                     -name,
                     area_id = value) %>% 
  filter(!is.na(area_id)) %>% 
  mutate(area_id = as.integer(area_id)) 

priorities_tbl <- priorities_base_tbl %>% 
  distinct(priority_id, priority_description) 

# lookup between priorities and areas
priority_area_tbl <- priorities_base_tbl %>% 
  select(-priority_description)


























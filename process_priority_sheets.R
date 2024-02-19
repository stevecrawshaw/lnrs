pacman::p_load(tidyverse,
               glue,
               janitor,
               fs,
               readxl,
               writexl,
               rgbif,
               httr2,
               rvest)

# Source Data ----
path = "data/Priorities and Measures Master for portal_Updated working version.xlsx"
sheets_vec <-
  readxl::excel_sheets(path)

sfi_raw_tbl <- read_csv("data/sfi_raw.csv")

# Utility Functions ----


# Function to check URL
check_url <- function(url) {
  tryCatch({
    # Create and perform the request
    response <- request(url) |>
      req_perform()
    
    # Check the status code
    status_code <- resp_status(response)
    
    # Determine if the link is valid based on the status code
    if (status_code == 200) {
      TRUE
    } else {
      FALSE
    }
  }, error = function(e) {
    FALSE
  })
}


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
    path = "data/Priorities and Measures Master for portal_working version.xlsx",
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
           area_description = brief_narrative,
           area_link = `links_to_further_info_guidance`) %>%
    mutate(
      area_id = as.integer(area_id),
      area_name = map_chr(area_name, ~ clean_text(.x))
    ) %>% 
    filter(!is.na(area_id))
}

parse_priorities_tbl <- function(sheets_list, areas_count) {
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
  path <-  "data/portal_upload/"
  nms <-  paste0(path, names(tbl_list), ".csv")
  walk2(tbl_list, nms, ~write_csv2(.x, .y, na = ""))
  # excel for viewing
  write_xlsx(tbl_list, path = glue("{path}main_sheets.xlsx"))
  
}

# Use chat GPT to get the Linnaean names https://chat.openai.com/share/086bf029-f2a7-409a-b60c-a5964015df21

make_priority_species_tbl <- function(sheets_list){
sheets_list %>% 
  pluck("priority_species") %>% 
  rownames_to_column(var = "species_id") %>% 
  mutate(species_id = as.integer(species_id)) %>% 
    rename(linnaean_name = linnaean)
}

get_gbif_tbl <- function(priority_species_tbl){
# get the gbif definitive species data
  # If this fails ***********CHECK VPN***********
  test_status <- function(){
    
    status_code <- request("https://api.gbif.org/v1/species/match") %>% 
      req_headers("Accept" = "application/json") %>% 
      req_url_query(verbose = FALSE,
                    name = "Apus apus") %>% 
      req_perform() %>% 
      resp_status()
    
    if (status_code[1] == 200L) TRUE else FALSE
    
  }
  if ( isTRUE(test_status())) {
priority_species_tbl %>% 
  pull(linnaean_name) %>% 
  name_backbone_checklist() %>% 
  select(-rank, -confidence, -matchType, acceptedUsageKey) %>% 
  mutate(gbif_species_url = glue("https://www.gbif.org/species/{usageKey}"))
  } else {
      print("Problem accessing GBIF - check VPN!")
  }
}

make_species_area_priority_lookup_tbl <- function(priority_species_tbl){
  
priority_species_tbl %>% 
  separate_longer_delim(cols = most_relevant_areas,
                        delim = "; ") %>%
  separate_longer_delim(cols = relevant_priorities,
                        delim = "; ") %>%
  rename(area_id = most_relevant_areas,
         priority_id = relevant_priorities) %>%
  distinct(species_id, area_id, priority_id) %>% 
  mutate(
    across(
      everything(),
      ~if_else(!str_starts(.x, "[0-9]"),
               NA_integer_,
               as.integer(.x))
      )
    )
}

make_species_tbl <- function(priority_species_tbl, gbif_tbl){
priority_species_tbl %>% 
  select(-species, -relevant_priorities, -most_relevant_areas, -link_to_further_guidance, -linnaean_name) %>% 
  inner_join(gbif_tbl, by = join_by(species_id == verbatim_index))
}

parse_cs_grant_codes <- function(sheets_list){

sheets_list %>% 
    pluck("farming_codes") %>% 
    filter(scheme == "Countryside Stewardship") %>% 
    mutate(category = str_to_sentence(meaning),
         meaning = NULL,
         scheme = NULL) %>% 
    rename(code_prefix = code)
}

# Grants ----

# construct tables of grants and links to be joined to recommendations
# Countryside Stewardship Grant Links Table ----
make_url_vec <- function(base_url = "https://www.gov.uk/countryside-stewardship-grants", num_pages = 6){
  
  pages_url <- paste0(rep(base_url, num_pages -1), "?page=", 2:num_pages)
  
  return(c(base_url, pages_url))
}

get_links <- function(url){
  # Read the HTML content of the page
  page <- read_html(url)
  # Find all link nodes
  link_nodes <- html_nodes(page, "a")
  # output a tibble
  tibble(
    text = html_text(link_nodes),
    url = html_attr(link_nodes, "href"))
}

make_links_raw_tbl <- function(make_url_vec, get_links){
  make_url_vec() %>% 
    map(get_links) %>% 
    bind_rows()
}

make_cs_tbl <- function(links_raw_tbl, domain = "https://www.gov.uk"){
  # clean and wrangle the links to get just the guidance links
  links_raw_tbl %>% 
    mutate(grant_name = str_trim(text, side = "both")) %>% 
    # need to filter for 2 cap letters, numbers colon **and** 2 cap letters, numbers, space
    filter(str_detect(grant_name, pattern = "^[A-Z]{2}\\d{0,2}:|^[A-Z]{2}\\d{0,2}\\s")) %>% 
    mutate(url = glue("{domain}{url}"), 
           text = NULL,
           grant_id = str_extract(grant_name, "^[^:]+") %>% 
             str_extract("^\\w+"),
           grant_scheme = "Countryside Stewardship")
  # flipping missing colon BC3 BC4
}
# Sustainable Farming Initiative Grants Links Table ----

# from Table 1 here https://assets.publishing.service.gov.uk/media/6516c0986a423b0014f4c62e/SFI23_handbook.pdf
# via chatGPT to parse into table 
# https://chat.openai.com/c/d5c6018e-f618-4cf3-95df-f9d66fef9e65

clean_sfi_tbl <- function(sfi_raw_tbl, url = "https://assets.publishing.service.gov.uk/media/6516c0986a423b0014f4c62e/SFI23_handbook.pdf"){
  
  sfi_raw_tbl %>% 
    mutate(url = url,
           grant_name = glue("{code}: {sfi_action}"),
           grant_scheme = "Sustainable Farming Initiative",
           grant_id = code,
           category = str_remove(category, "Actions for ") %>% 
             str_to_sentence()
    ) %>% 
    select(url, grant_name, grant_id,
           grant_scheme, annual_payment,
           category) 
  
}

make_all_grants_tbl <- function(cs_tbl, sfi_tbl, cs_grant_codes_tbl){
  
  bind_rows(cs_tbl, sfi_tbl) %>% 
    mutate(code_prefix = if_else(str_starts(grant_id, "[A-Z]{2}"),
                                 str_extract_all(grant_id, "[A-Z]") %>% 
                                   map_chr(~paste0(.x, collapse = "")),
                                 "")) %>% 
    left_join(cs_grant_codes_tbl,
              by = join_by(code_prefix == code_prefix)) %>% 
    mutate(grant_focus = coalesce(category.x, category.y),
           across(starts_with("category"), ~NULL),
           code_prefix = NULL)

}

# Test functions and generate data ----

sheets_list <- make_list_from_sheets(path, sheets_vec)


priority_species_tbl <- make_priority_species_tbl(sheets_list)
gbif_tbl <- get_gbif_tbl(priority_species_tbl)
species_tbl <- priority_species_tbl %>% 
  make_species_tbl(gbif_tbl)

species_area_priority_lookup_tbl <- priority_species_tbl %>% 
  make_species_area_priority_lookup_tbl()

areas_tbl_raw <- parse_areas_tbl(sheets_list)

areas_tbl <- areas_tbl_raw %>% 
  select(-funding_schemes)

area_funding_schemes_tbl <- areas_tbl_raw %>% 
  select(area_id, funding_schemes) %>% 
  separate_longer_delim(cols = funding_schemes, delim = "\r\n\r\n") %>% 
  filter(!is.na(funding_schemes)) %>% 
  mutate(valid  = map_lgl(funding_schemes, check_url)) %>% 
  filter(valid) %>% 
  select(-valid)

priorities_tbl <- parse_priorities_tbl(sheets_list, areas_count = 53) %>% 
  distinct(theme, priority_id, biodiversity_priority)

measures_tbl <- parse_measures_tbl(sheets_list) %>% 
  distinct(measure_id, measure, ambition, land_type)

priority_area_lookup_tbl <- parse_priorities_tbl(sheets_list, areas_count = 53) %>% 
  distinct(area_id, priority_id)

measures_priority_area_lookup_tbl <- parse_measures_tbl(sheets_list) %>% 
  distinct(measure_id, area_id, priority_id, link_or_code)
# Grants
links_raw_tbl <- make_links_raw_tbl(make_url_vec, get_links)
cs_tbl <- make_cs_tbl(links_raw_tbl = links_raw_tbl,
                               domain = "https://www.gov.uk")
# all urls good?
map_lgl(cs_tbl$url, check_url) %>% 
  all()

sfi_tbl <- clean_sfi_tbl(sfi_raw_tbl = sfi_raw_tbl)

map_lgl(sfi_tbl$url, check_url) %>% 
  all()

cs_grant_codes_tbl <- parse_cs_grant_codes(sheets_list)
# Consolidate grant data
all_grants_tbl <- make_all_grants_tbl(cs_tbl,
                                      sfi_tbl, 
                                      cs_grant_codes_tbl)

# Write Data ----


tbl_list <- list(
  "lnrs-areas-tbl" = areas_tbl, 
  "lnrs-priorities-tbl" = priorities_tbl, 
  "lnrs-measures-tbl" = measures_tbl,
  "lnrs-species-tbl" = species_tbl,
  "lnrs-priority-area-lookup_tbl" = priority_area_lookup_tbl,
  "lnrs-measures-priority-area-lookup-tbl" = measures_priority_area_lookup_tbl,
  "lnrs-species-area-priority-lookup-tbl" = species_area_priority_lookup_tbl,
  "lnrs-all-grants-tbl" = all_grants_tbl,
  "lnrs-area-funding-schemes-tbl" = area_funding_schemes_tbl
                 )

save_tbls(tbl_list)

rlang::as_string(quote(tbl_list))



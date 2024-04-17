pacman::p_load(tidyverse,
               glue,
               janitor,
               fs,
               readxl,
               writexl,
               rgbif,
               httr2,
               rvest,
               rlist)

# Source Data ----
# April 2024
path = "data/NEW_Priorities and Measures Master for portal_updated with measures vs area.xlsx"

sheets_vec <-
  readxl::excel_sheets(path)

sfi_raw_tbl <- read_csv("data/sfi_raw.csv")

make_list_from_sheets <- function(
    path = "data/NEW_Priorities and Measures Master for portal_updated with measures vs area.xlsx",
    sheets_vec) {
  # read all the sheets and store in a list with each element a sheet
  sheets_vec %>%
    map(~ read_xlsx(path, .x)) %>%
    set_names(make_clean_names(sheets_vec)) %>%
    map(.f = clean_names)
}

sheets_list <- make_list_from_sheets(path, sheets_vec)


# Utility Functions ----

check_valid_grant_string <- function(x, letters = 2){
  
  if (letters == 2){
    pattern = "^[A-Z]{2}\\d+$"
  } else if (letters == 3) {
    pattern = "^[A-Z]{3}\\d+$"
  }
  str_detect(x, pattern)
}


save_tbls <- function(tbl_list, path = "data/portal_upload/"){
  # take a named list and write csv's for the portal 
  # and an excel file for introspection
  # csv - semicolon delims
  nms <-  paste0(path, names(tbl_list), ".csv")
  walk2(tbl_list, nms, ~write_csv2(.x, .y, na = ""))
  # excel for viewing
  write_xlsx(tbl_list, path = glue("{path}main_sheets.xlsx"))
  
}

add_id <- function(tbl) {
  # helper to add autonumber integer
  tbl %>% 
    rownames_to_column("id") %>% 
    mutate(id = as.integer(id))
}
  
check_url <- function(url) {
  tryCatch({
    # Create and perform the request
    response <- request(url) |>
      req_perform()
    
    # Check the status code
    resp_status(response)
    

  }, error = function(e) {
    0
  })
}

linkchecker <- function(x){
  # check if a link is valid
  # return a character string 200 if ok, otherwise the http_*
  # R error code as httr2 doesn't return the native 
  # API error code
  tryCatch(
    expr = {
      request(x) %>% 
        req_perform() %>% 
        resp_status() %>% 
        as.character()
      #message("Successfully checked the link.")
    },
    error = function(e){
      #message('Caught an error!')
      class(e)[1] 
    },
    warning = function(w){
      #message('Caught an warning!')
      class(w)[1]
    },
    finally = {
      #message('All done, quitting.')
    }
  )    
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

#    Grant functions ----

make_grants_tbl <- function(cs_tbl, sfi_tbl, cs_grant_codes_tbl){
  
  bind_rows(cs_tbl, sfi_tbl) %>% 
    mutate(code_prefix = if_else(str_starts(grant_id, "[A-Z]{2}"),
                                 str_extract_all(grant_id, "[A-Z]") %>% 
                                   map_chr(~paste0(.x, collapse = "")),
                                 "")) %>% 
    left_join(cs_grant_codes_tbl,
              by = join_by(code_prefix == code_prefix)) %>% 
    mutate(grant_focus = coalesce(category.x, category.y),
           across(starts_with("category"), ~NULL),
           code_prefix = NULL,
           link_status = NULL) %>% 
    add_id() %>% 
    relocate(grant_id, id, url, grant_name, grant_focus, grant_scheme, annual_payment) %>% 
    mutate(grant_summary = glue(
      "Grant name: {grant_name}\n
     Grant focus: {grant_focus}\n
     Grant scheme: {grant_scheme}\n
     Link: <a href={url} target=_blank>{url}</a>\n
     url: {url}"))
  
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

# construct tables of grants and links to be joined to measures
# Countryside Stewardship Grant Links Table
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
# SFI Grants Links Table 

# from Table 1 here https://assets.publishing.service.gov.uk/media/6516c0986a423b0014f4c62e/SFI23_handbook.pdf
# via chatGPT to parse into table 
# https://chat.openai.com/c/d5c6018e-f618-4cf3-95df-f9d66fef9e65

clean_sfi_tbl <- function(sfi_raw_tbl, url = "https://assets.publishing.service.gov.uk/media/661794066ad004cd0fac05ed/SFI23_handbook_v5.0.pdf"){
  
  if(check_url(url) != 200){
    stop("URL not found")
  }
  
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

#    Input parsing functions ----

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


# Measures table functions ----
# measures table processing functions
# three in all
# first one does some renaming as the sheet names are not clean

rename_dirty_sheet <- function(sheets_list,
                               tbl = "measures_by_area",
                               end_of_first = 13,
                               end_of_second = 65,
                               measures_n = 153 
){
  # rename the measures_by_area sheet
  measures_by_area_interim_tbl <- sheets_list %>% 
    pluck(tbl)
  
  first_half_range <- 1:end_of_first
  second_half_range <- (end_of_first + 1):end_of_second
  
  first_half_names <- measures_by_area_interim_tbl[1, first_half_range] %>% 
    as.character() %>% 
    make_clean_names()
  
  second_half_names <- measures_by_area_interim_tbl[1, second_half_range] %>%
    names() %>% 
    map_chr(~str_extract(.x, "[0-9]+")) 
  
  all_names <- c(first_half_names, second_half_names)  
  
  measures_by_area_interim_tbl[2:measures_n,] %>% 
    set_names(all_names)
  
}

make_measures_area_tbl_1 <- function(measures_area_tbl_interim){
  # make the first measures table
  # take out some redundant fields and replace N/A with NA
  measures_area_tbl_interim %>%
    rownames_to_column("area_measure_id") %>% 
    mutate(area_measure_id = as.integer(area_measure_id),
           across(.cols = c(level_of_ambition,
                            land_type,
                            stakeholder,
                            sfi,
                            relevant_map_layer,
                            link_to_further_guidance),
                  ~na_if(.x, "N/A")),
           across(.cols = c(woodland, other), ~NULL)
    ) 
}

make_measures_area_tbl <- function(measures_area_tbl_1,
                                   areas_start_col = 13){
  # assign the areas to the measures turning x's in columns into 
  # area_ids
  # separate out the land type and stakeholder columns
  measures_area_tbl_1 %>%
    mutate(across(
      .cols = all_of(areas_start_col:last_col()),
      ~ if_else(.x == "x", cur_column(),
                NA_character_)
    )) %>%
    pivot_longer(cols = all_of(areas_start_col:last_col()), values_to = "area_id", names_to = NULL) %>% 
    filter(!is.na(area_id)) %>% 
    separate_longer_delim(cols = c(land_type), delim = "; ") %>% 
    separate_longer_delim(cols = stakeholder, delim = "; ") %>% 
    rename(priority_id = associated_priority_code) %>% 
    mutate(across(.cols = ends_with("_id"), as.integer))
  
}

make_measures_area_long_tbl <- 
  function(measures_area_tbl,
           area_schemes_condensed_tbl,
           priorities_tbl){
    
    measures_area_tbl %>% 
      separate_longer_delim(cols = countryside_stewardship, delim = "; ") %>% 
      separate_longer_delim(cols = sfi, delim = "; ") %>% 
      mutate(cs_id = if_else(
        check_valid_grant_string(countryside_stewardship, letters = 2),
        countryside_stewardship,
        NA_character_),
        countryside_stewardship = NULL,
        sfi_id = if_else(
          check_valid_grant_string(sfi, letters = 3),
          sfi,
          NA_character_),
        sfi = NULL) %>% 
      pivot_longer(cols = c(sfi_id, cs_id),
                   names_to = NULL,
                   values_to = "grant_id") %>% 
      filter(!is.na(grant_id)) %>% 
      distinct() %>% 
      left_join(area_schemes_condensed_tbl,
                by = join_by(area_id == area_id)) %>% 
      select(-theme) %>% 
      left_join(priorities_tbl %>% select(priority_id, theme),
                by = join_by(priority_id == priority_id)) 
  }

# Species functions ----

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
      mutate(gbif_species_url = glue("https://www.gbif.org/species/{usageKey}")) %>% 
      clean_names()
  } else {
    print("Problem accessing GBIF - check VPN!")
  }
}

make_species_tbl <- function(priority_species_tbl, gbif_tbl){
  priority_species_tbl %>% 
    select(-relevant_priorities, -link_to_further_guidance, -linnaean_name) %>% 
    rename(common_name = species) %>% 
    inner_join(gbif_tbl,
               by = join_by(species_id == verbatim_index))  
}



# Ingest and shape the data ----

links_raw_tbl <- make_links_raw_tbl(make_url_vec, get_links)

cs_tbl <- make_cs_tbl(links_raw_tbl = links_raw_tbl,
                      domain = "https://www.gov.uk") %>% 
  mutate(link_status = map_int(url, check_url))

# TRUE if all links good
if(all(cs_tbl$link_status == 200)){
  TRUE
} else {
  cs_tbl %>% filter(link_status != 200)
}

sfi_tbl <- clean_sfi_tbl(sfi_raw_tbl = sfi_raw_tbl)

cs_grant_codes_tbl <- parse_cs_grant_codes(sheets_list)
#     Consolidate grant data ----

grants_tbl <- make_grants_tbl(cs_tbl,
                              sfi_tbl, 
                              cs_grant_codes_tbl) 

grants_tbl %>% glimpse()

#     Areas and Funding Schemes ----

interim_areas_tbl <- parse_areas_tbl(sheets_list)

# nested tbl to capture data with valid and failing urls

  scheme_check_url_tbl <- interim_areas_tbl %>% 
  select(area_id, area_name, funding_schemes) %>% 
  separate_longer_delim(cols = funding_schemes, delim = "\r\n\r\n") %>% 
  filter(!is.na(funding_schemes)) %>% 
  mutate(status = map_chr(funding_schemes, linkchecker)) %>% 
  nest_by(status)
  
  # failed urls for introspection
failed_urls <- scheme_check_url_tbl %>% 
  filter(status == "httr2_http_404") %>% 
  unnest(data) %>% 
  ungroup() %>% 
  pull(funding_schemes)

failed_urls

area_funding_schemes_tbl <- scheme_check_url_tbl %>% 
  filter(status != "httr2_http_404") %>%
  unnest(data) %>%
  ungroup() %>% 
  select(-status) %>%
  add_id() %>% 
  relocate(id, area_id, area_name, funding_schemes)

area_funding_schemes_tbl %>% glimpse()

area_funding_schemes_tbl %>% 
  write_csv("data/portal_upload/lnrs-area-funding-schemes-tbl.csv")

areas_tbl <- interim_areas_tbl %>% 
  select(-funding_schemes)

areas_tbl %>% glimpse()

#     Priorities ----

parse_priorities_tbl <- function(sheets_list, areas_tbl, areas_start_col = 5) {
  # read and process the priorities table. 
  # amend area_count var if new areas are added
  
  priorities_raw_tbl <-
    sheets_list %>% pluck("statement_of_bd_priorities")
  
  area_ids_vec <- areas_tbl$area_id
  
  nms <- c("theme",
           "priority_id",
           "biodiversity_priority",
           "simplified_biodiversity_priority",
           area_ids_vec)
  
    priorities_raw_tbl %>%
    filter(x1 != "Theme") %>%
    set_names(nms) %>%
    mutate(across(
      .cols = all_of(areas_start_col:last_col()),
      ~ if_else(.x == "x", cur_column(),
                NA_character_)
    )) %>%
    pivot_longer(cols = all_of(areas_start_col:last_col()), values_to = "area_id") %>%
    select(-name) %>%
    filter(!is.na(area_id)) %>%
    mutate(across(.cols = ends_with("_id"), as.integer))
}

priorities_area_tbl <- parse_priorities_tbl(sheets_list,
                                       areas_tbl,
                                       areas_start_col = 5)

priorities_areas_lookup_tbl <- priorities_area_tbl %>% 
  distinct(priority_id, area_id) %>% 
  add_id() %>% 
  relocate(id, priority_id, area_id)

priorities_areas_lookup_tbl %>% glimpse()

priorities_tbl <- priorities_area_tbl %>% 
  select(-area_id) %>% 
  distinct(theme,
           priority_id,
           biodiversity_priority,
           simplified_biodiversity_priority) %>% 
  relocate(priority_id,
           theme,
           biodiversity_priority,
           simplified_biodiversity_priority)

priorities_tbl %>% glimpse()

# separate out measures tables, one by priority, one by area
# measures by PRIORITY ----
# Deprecated as a component of the data since April 2024
# measures_by_priority_interim_tbl <- 
#   sheets_list %>% 
#   pluck("measures_by_priority") %>% 
#   rownames_to_column(var = "measure_id") %>%
#   mutate(
#     priority_measure_id = as.integer(measure_id),
#     measure_id = NULL,
#     across(.cols = c(level_of_ambition, land_type),
#            ~na_if(.x, "N/A")),
#     associated_priority_number_1 = as.character(associated_priority_number_1),
#     priority_id = if_else(
#            !is.na(other_relevant_priorities),
#            glue("{associated_priority_number_1},{other_relevant_priorities}"),
#            associated_priority_number_1),
#     associated_priority_number_1 = NULL,
#     other_relevant_priorities = NULL
#          ) %>% 
#   rename(measure = recommended_measure)
# 
# 
# priority_measures_grant_interim_tbl <- 
#   measures_by_priority_interim_tbl %>% 
#   select(priority_measure_id,
#          countryside_stewardship,
#          sfi) %>% 
#   mutate(cs_clean = if_else(
#     check_valid_grant_string(countryside_stewardship),
#     countryside_stewardship,
#     NA_character_),
#     sfi_clean = if_else(
#       check_valid_grant_string(sfi),
#       sfi,
#       NA_character_),
#     countryside_stewardship = NULL,
#     sfi = NULL) %>% 
#   separate_longer_delim(cs_clean, "; ") %>% 
#   separate_longer_delim(sfi_clean, "; ") 
#   
# 
# make_priority_measures_grant_lookup_tbl <- function(priority_measures_grant_interim_tbl){
#   # collate the 2 types of grant by priority_measures_id
#   sfi <- priority_measures_grant_interim_tbl %>% 
#     select(priority_measure_id, grant_id = sfi_clean) %>% 
#     filter(!is.na(grant_id))
#   
#   cs <- priority_measures_grant_interim_tbl %>% 
#     select(priority_measure_id, grant_id = cs_clean) %>% 
#     filter(!is.na(grant_id))
# bind_rows(sfi, cs)  %>% 
#   add_id() %>% 
#   relocate(id, priority_measure_id, grant_id)
#   
# }
# 
# 
# priority_measures_grants_lookup_tbl <- make_priority_measures_grant_lookup_tbl(priority_measures_grant_interim_tbl)
# 
# 
# priority_measures_tbl  <- 
#   measures_by_priority_interim_tbl %>% 
#   select(-c(priority_id, other, woodland, sfi, countryside_stewardship)) %>% 
#   relocate(priority_measure_id, measure,everything())
# 
# priority_measures_tbl %>% glimpse()
  

# measures by AREA ----
# make the measures by area table

#    area funding schemes condensed ----

area_schemes_condensed_tbl <- area_funding_schemes_tbl %>% 
  group_by(area_id) %>% 
  summarise(scheme = paste0(funding_schemes, collapse = "\n"))

#     Make the area measures tbl for ODS ----

# lets try this as an example to test filtering
area_measures_long_tbl <- 
  rename_dirty_sheet(sheets_list,
                     tbl = "measures_by_area",
                     end_of_first = 13,
                     end_of_second = 65,
                     measures_n = 153 ) %>% 
  make_measures_area_tbl_1() %>% 
    make_measures_area_tbl(areas_start_col = 13) %>% 
  make_measures_area_long_tbl(area_schemes_condensed_tbl, priorities_tbl) 

areas_measures_grants_lookup_tbl <- area_measures_long_tbl %>% 
  distinct(area_measure_id, grant_id)

# this is the table written to ods v

area_measures_tbl <- area_measures_long_tbl %>% 
  left_join(grants_tbl %>% select(grant_id, url),
            by = join_by(grant_id == grant_id)) %>% 
  group_by(area_measure_id, theme, priority_id, biodiversity_priority, measure, level_of_ambition, land_type, 
           stakeholder, relevant_map_layer, link_to_further_guidance, area_id, scheme) %>%
  summarise(grant_link = paste0(url, collapse = "\n"), .groups = "drop") %>% 
  left_join(areas_tbl %>% select(area_id, area_name),
            by = join_by(area_id == area_id)) 


area_measures_tbl %>%
  write_csv("data/area_measures_long_tbl.csv", na = "")

# retry
# this might be the one
test_tbl <- area_measures_long_tbl %>%  
  left_join(grants_tbl,
            by = join_by(grant_id == grant_id)) %>% 
  # group_by(area_measure_id, theme, priority_id, biodiversity_priority, measure, level_of_ambition, land_type, 
  #          stakeholder, relevant_map_layer, link_to_further_guidance, area_id, scheme) %>%
  # summarise(grant_link = paste0(url, collapse = "\n"), .groups = "drop") %>% 
  left_join(areas_tbl %>% select(area_id, area_name),
            by = join_by(area_id == area_id))


# Test functions and generate data ----

priority_species_tbl <- make_priority_species_tbl(sheets_list)
gbif_tbl <- get_gbif_tbl(priority_species_tbl)

species_tbl <- priority_species_tbl %>% 
  make_species_tbl(gbif_tbl)

species_priority_lookup_tbl <- 
  priority_species_tbl %>% 
  select(species_id, relevant_priorities) %>% 
  separate_longer_delim(relevant_priorities, ", ") %>% 
  mutate(priority_id = as.integer(relevant_priorities),
         relevant_priorities = NULL) %>% 
  add_id() %>% 
  relocate(id, priority_id, species_id)

species_priority_lookup_tbl %>% glimpse()
  
species_area_lookup_tbl <- sheets_list %>% 
  pluck("species_by_area") %>% 
  slice(2:n()) %>%
  mutate(across(
    .cols = all_of(3:last_col()),
    ~ if_else(.x == "x", cur_column(),
              NA_character_))) %>% 
  rename(species_id = x1,
         species = identifier) %>% 
  pivot_longer(cols = -c(species_id, species)) %>% 
  mutate(area_id = str_sub(value, 2, 3) %>%
           str_remove("_") %>% 
           as.integer()) %>%
  filter(!is.na(value)) %>% 
  transmute(species_id = as.integer(species_id), area_id) %>% 
  arrange(area_id) %>% 
  add_id() %>% 
  relocate(id, species_id, area_id)
  
# table to relate species, priorities and areas

species_priority_area_tbl <- species_tbl %>% 
  left_join(species_priority_lookup_tbl,
            by = join_by(species_id == species_id)) %>%
  left_join(species_area_lookup_tbl,
            by = join_by(species_id == species_id),
            relationship = "many-to-many") %>%
  left_join(priorities_tbl,
            by = join_by(priority_id == priority_id)) %>%
  left_join(areas_tbl,
            by = join_by(area_id == area_id)) %>%
  select(species_id, taxa, common_name, scientific_name,
         canonical_name, gbif_species_url, area_id, area_name, priority_id, biodiversity_priority, simplified_biodiversity_priority, theme) %>% 
  glimpse()
# 
# A data table to capture the relationships between species, priorities and areas. Users can filter by priority and area to see which species are important for both categories. Duplication is possible as a species may be important for both a given area and a given priority.

species_priority_area_tbl %>% write_csv("data/portal_upload/species-priority-area-tbl.csv", na = "")

# examples for ODS query when a user filters to see the species important for areas and priorities

species_for_an_area <- species_priority_area_tbl %>% 
  filter(area_id == 1) %>% 
  group_by(common_name) %>%
  summarise(common_name = first(common_name),
            .groups = "drop") %>% 
  glimpse()

species_for_a_priority <- species_priority_area_tbl %>% 
  filter(priority_id == 1) %>% 
  group_by(common_name) %>%
  summarise(common_name = first(common_name),
            .groups = "drop") %>% 
  glimpse()



areas_count = nrow(areas_tbl)

# Write Data ----


tbl_list <- list(
  "areas-tbl" = areas_tbl, 
  "priorities-tbl" = priorities_tbl, 
  "species-tbl" = species_tbl,
  "area-measures-tbl" = area_measures_tbl,
  "priority-measures-tbl" = priority_measures_tbl,
  "priorities-areas-lookup-tbl" = priorities_areas_lookup_tbl,
  "priorities-measures-lookup-tbl" = priorities_measures_lookup_tbl,
  "priorities-areas-measures-lookup-tbl" = priorities_areas_measures_lookup_tbl,
  "species-area-lookup-tbl" = species_area_lookup_tbl,
  "species-priority-lookup-tbl" = species_priority_lookup_tbl,
  "area-funding-schemes-tbl" = area_funding_schemes_tbl,
  "priority-measures-grants-lookup-tbl" = priority_measures_grants_lookup_tbl,
  "areas-measures-grants-lookup-tbl" = areas_measures_grants_lookup_tbl,
  "grants-tbl" = grants_tbl
                 )

write_rds(tbl_list, "data/portal_tbl_list.rds")

save_tbls(tbl_list, path = "data/portal_upload/")
# print the CORE tbls
base_tbls_list <- base::setdiff(tbl_list,
tbl_list %>% 
  list.match("lookup")
)
map(base_tbls_list, names)


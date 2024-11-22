pacman::p_load(tidyverse,
             glue,
             janitor,
             readxl,
             writexl,
             rgbif,
             httr2,
             rvest,
             sf)

# Source Data ----
path = "data/Priorities and Measures Master for portal_for upload to app.xlsx"

sheets_vec <-
readxl::excel_sheets(path)

make_list_from_sheets <- function(
  path = path,
  sheets_vec) {
# read all the sheets and store in a list with each element a sheet
sheets_vec |>
  map(~ read_xlsx(path, .x)) |>
  set_names(make_clean_names(sheets_vec)) |>
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

upload_path <- "data/portal_upload/"

save_tbls <- function(tbl_list, path = upload_path){
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
tbl |> 
  rownames_to_column("id") |> 
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
    request(x) |> 
      req_perform() |> 
      resp_status() |> 
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
output_string <- input_string |>
  str_replace_all("(?<!\\s)-(?!\\s)", placeholder) |>
  # Step 2: Replace commas, double spaces, and semicolons with spaces
  str_replace_all("[,;]|\\s{2,}", " ") |>
  # Step 3: Replace the unique placeholder with a hyphen with space padding
  str_replace_all(placeholder, " - ")
return(output_string)
}

rename_action_col <- function(tbl){
  # utility function to rename the disparate action columns
  tbl |> 
    rename_with(\(x) "sfi_action",
                .cols = starts_with("SFI")) 
}

#    Grants ----

make_grants_tbl <- function(cs_tbl,
                          sfi_tbl,
                          ofc_clean_tbl,
                          cs_grant_codes_tbl){

bind_rows(cs_tbl, sfi_tbl, ofc_clean_tbl) |> 
  mutate(code_prefix = if_else(str_starts(grant_id, "[A-Z]{2}"),
                               str_extract_all(grant_id, "[A-Z]") |> 
                                 map_chr(~paste0(.x, collapse = "")), "")) |> 
  left_join(cs_grant_codes_tbl,
            by = join_by(code_prefix == code_prefix)) |> 
  mutate(code_prefix = NULL,
         link_status = NULL) |> 
  add_id() |> 
  relocate(grant_id, id, url, grant_name, grant_scheme) |> 
  mutate(grant_summary = glue(
    "Grant name: {grant_name}\n
   Grant scheme: {grant_scheme}\n
   Link: <a href={url} target=_blank>{url}</a>\n
   url: {url}"))

}

parse_cs_grant_codes <- function(sheets_list){

sheets_list |> 
  pluck("farming_codes") |> 
  filter(scheme == "Countryside Stewardship") |> 
  mutate(category = str_to_sentence(meaning),
         meaning = NULL,
         scheme = NULL) |> 
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
  make_url_vec() |> 
    map(get_links) |> 
    bind_rows()
}

make_cs_tbl <- function(links_raw_tbl, domain = "https://www.gov.uk"){
  # clean and wrangle the links to get just the guidance links
  links_raw_tbl |> 
    mutate(grant_name = str_trim(text, side = "both")) |> 
    # need to filter for 2 cap letters, numbers colon **and** 2 cap letters, numbers, space
    filter(str_detect(grant_name, pattern = "^[A-Z]{2}\\d{0,2}:|^[A-Z]{2}\\d{0,2}\\s")) |> 
    mutate(url = glue("{domain}{url}"), 
           text = NULL,
           grant_id = str_extract(grant_name, "^[^:]+") |> 
             str_extract("^\\w+"),
           grant_scheme = "Countryside Stewardship")
# flipping missing colon BC3 BC4
}


sfi_2024_url <- "https://www.gov.uk/government/publications/sustainable-farming-incentive-scheme-expanded-offer-for-2024/sfi-scheme-information-expanded-offer-for-2024#annex-b-summary-of-the-initial-expanded-sfi-offer-from-summer-2024"

make_sfi_web_tbl <- function(sfi_url = sfi_2024_url){

rvest::read_html(sfi_url) |> 
html_table() |> 
keep(~ncol(.x) == 4) |> 
map(rename_action_col) |> 
bind_rows() |> 
clean_names() |> 
rename(grant_id = code)

}

sfi_web_tbl <- make_sfi_web_tbl(sfi_2024_url)
# funding source links

find_funding_base_url <- "https://www.gov.uk/find-funding-for-land-or-farms"

funding_base_urls <- paste0(find_funding_base_url, c("?page=1", "?page=2", "?page=3"))

get_sfi_vector <- function(funding_base_url){
#extract the list of grant links given a page of grants
funding_base_url |> 
  read_html() |> 
  html_elements(css = ".gem-c-document-list__item  ") |> 
  html_text2()
}

get_sfi_stub <- function(item){
# get the first item which contains the grant code, summary
# and can be used to create the URL for the grant
item |> 
  str_split("\n") |> 
  pluck(1, 1)
}

make_url_from_stub <- function(stub, base_url){
# do various cleaning operations on the stub
# and make a valid url for the grant
stub_reformatted <- stub |> 
  str_replace_all(pattern = ":\\s|\\s",
                  replacement = "-") |> 
  str_remove_all("\\(|\\)|,")|>
  str_replace("%", "-percent") |>
  str_remove("–") |> # this dash is not the same as the others
  str_replace("--", "-") |> 
  tolower()

paste0(base_url, "/", stub_reformatted)
}

grant_name <- funding_base_urls |> 
map(get_sfi_vector) |> 
map(~map_chr(.x, get_sfi_stub)) |> 
unlist() 

grant_urls <- grant_name |> 
map_chr(~make_url_from_stub(.x, find_funding_base_url))

# lcheck <- map_chr(t, linkchecker) |> 
#   map_lgl(~.x == "200")
# 
# t[!lcheck]

sfi_funding_tbl <- tibble(grant_name, url = grant_urls) |> 
separate(grant_name,
         into = c("grant_id", "summary"),
         sep = ": ",
         remove = FALSE) |> 
mutate(grant_scheme = "SFI",
       summary_wrapped = str_wrap(summary,
                                  whitespace_only = TRUE, 
                                  width = 50),
       link_status = map(url, linkchecker) |>
         as.integer()) |> 
glimpse()

sfi_tbl <- sfi_web_tbl |> 
left_join(sfi_funding_tbl,
          by = join_by(grant_id)) |> 
select(-c(sfi_action, annual_payment, action_s_duration)) |> 
glimpse()

# countryside stewardship and make final grants tbl

links_raw_tbl <- make_links_raw_tbl(make_url_vec, get_links)

cs_tbl <- make_cs_tbl(links_raw_tbl = links_raw_tbl,
                    domain = "https://www.gov.uk") |> 
mutate(link_status = map_int(url, check_url))

# TRUE if all links good
if(all(cs_tbl$link_status == 200)){
TRUE
} else {
cs_tbl |> filter(link_status != 200)
}

cs_grant_codes_tbl <- parse_cs_grant_codes(sheets_list)

# other funding codes
# 
ofc_raw_tbl <- sheets_list |> 
  pluck("other_funding_codes")

ofc_clean_tbl <- ofc_raw_tbl |> 
  transmute(grant_id = code,
         grant_name = glue("{grant_id}: {fund_name}"),
         url = link)

#     Consolidate grant data 

grants_tbl <- make_grants_tbl(cs_tbl,
                            sfi_tbl, 
                            ofc_clean_tbl,
                            cs_grant_codes_tbl) |> 
glimpse()

# Habitat Creation and Restoration ----
# 
parse_hab_sheet <- function(sheets_list, sheet_name){

  # read and process the habitat creation and restoration sheets
  # relate the area_ids to the habitat types and return a tidy table
  # with the habitat types condensed into a single column
  out_tbl <- sheets_list |> 
  pluck(sheet_name) |> 
  mutate(across(
    .cols = all_of(2:last_col()),
    ~ if_else(
      .x %in% c("x", "X"),
      str_remove(cur_column(), "x") |>
        as.integer(),
      NA_integer_))) |> 
  filter(identifier != "Habitat") |> 
    pivot_longer(cols = -identifier,
                 names_to = "col",
                 values_to = "area_id") |>
    filter(!is.na(area_id)) |>
    select(-col) |>
    group_by(area_id) |>
    summarise(habitat = paste(identifier, collapse = ":\n")) |>
    arrange(area_id)
  names(out_tbl)[names(out_tbl) =="habitat"] <- sheet_name
  out_tbl

}

hab_creation_tbl <- parse_hab_sheet(sheets_list, "bng_hab_creation") |> 
  glimpse()

hab_mgt_tbl <- parse_hab_sheet(sheets_list, "bng_hab_mgt") |> 
  glimpse()

# Areas ----

parse_areas_tbl <- function(sheets_list) {
# import and process the priority areas
sheets_list |>
  pluck("areas_for_description") |>
  rename(area_id = identifier,
         area_name = title,
         area_description = brief_narrative,
         area_link = `links_to_further_info_guidance`) |>
  mutate(
    area_id = as.integer(area_id),
    area_name = map_chr(area_name, ~ clean_text(.x))
  ) |> 
  filter(!is.na(area_id))
}

interim_areas_tbl <- parse_areas_tbl(sheets_list)

make_area_funding_schemes_tbl <- function(interim_areas_tbl){

interim_areas_tbl  |>  
select(area_id, area_name, local_funding_schemes) |> 
separate_longer_delim(cols = local_funding_schemes,
                      delim = "; ") |> 
filter(!is.na(local_funding_schemes)) |> 
add_id() |> 
relocate(id, area_id, area_name, local_funding_schemes)
}

area_funding_schemes_tbl <- make_area_funding_schemes_tbl(interim_areas_tbl)

area_schemes_condensed_tbl <- area_funding_schemes_tbl |> 
group_by(area_id) |> 
summarise(local_funding_schemes = paste(local_funding_schemes, collapse = "\n"))

# just the shapes and id \ name - upload to the portal as geojson
areas_shape <- st_read("data/lnrs-sub-areas.fgb")

areas_tbl <- interim_areas_tbl |> 
mutate(area_link = str_replace_all(area_link, "; ", "\n")) |> 
select(-local_funding_schemes) |>
left_join(hab_mgt_tbl,
          by = join_by(area_id == area_id)) |>
  left_join(hab_creation_tbl,
          by = join_by(area_id == area_id)) |>
glimpse()


# areas_tbl |> 
# write_csv("data/lnrs-areas-tbl.csv", na = "")

# Priorities ----

make_priorities_tbl <- function(sheets_list,
                              bd_priorities_sheet_name = "statement_of_bd_priorities"){
# read and process the priorities table. 
# amend area_count var if new areas are added

priorities_raw_tbl <- sheets_list |> 
pluck("statement_of_bd_priorities")

nms <- priorities_raw_tbl[1,] |> 
as.character() |> 
make_clean_names()

priorities_raw_tbl  |> 
tail(-1) |> 
set_names(nms) |> 
mutate(priority_id = code |>
         as.integer(),
       code = NULL) |> 
glimpse()
}

priorities_tbl <- make_priorities_tbl(sheets_list)

# Measures ----
 
make_measures_raw_tbl <- function(
    sheets_list,
    measures_sheet_name = "measures_by_area",
    start_col_for_areas_index = 15){
# do lots of renaming operations to create consistent names
measures_raw_tbl <- sheets_list |> 
pluck(measures_sheet_name) 

(area_ids_raw <- names(measures_raw_tbl)[start_col_for_areas_index:dim(measures_raw_tbl)[2]])

# a vector of area id's with underscores and trailing numbers removed
(area_ids_new <- if_else(str_detect(area_ids_raw, "_"), str_extract(area_ids_raw, "^(.*?)_"), area_ids_raw )|> str_remove_all("_"))

(non_area_names_old <- measures_raw_tbl[1, 1:(start_col_for_areas_index - 1)] |> 
as.character())

(names(measures_raw_tbl)[1:start_col_for_areas_index -1] <- non_area_names_old |> 
make_clean_names())

(names(measures_raw_tbl)[start_col_for_areas_index:dim(measures_raw_tbl)[2]] <- area_ids_new
)
#names(measures_raw_tbl)
measures_raw_tbl |> 
  filter(!is.na(associated_priority_code))
}

make_area_measures_raw_tbl <- function(measures_raw_tbl){

measures_raw_tbl |> 
tail(-1) |>
rename(priority_id = associated_priority_code) |>
mutate(across(
  .cols = x1:last_col(),
  ~ if_else(.x == "x", cur_column(),
            NA_character_))) |> 
rownames_to_column("measure_id") |>
mutate(measure_id = as.integer(measure_id)) |>
mutate(across(starts_with("x"),
              ~str_remove(.x, "x") |> 
                as.integer())) 
}

make_area_measures_interim_tbl <- function(area_measures_raw_tbl){

area_measures_raw_tbl |> 
select(measure_id, starts_with("x"),
       priority_id,
       measure,
       other_priorities_delivered,
       core_supplementary, 
       mapped_unmapped, 
       measure_type, 
       stakeholder, 
       relevant_map_layer,
       link_to_further_guidance,
       countryside_stewardship,
       sfi, 
       other_funding) |> 
pivot_longer(cols = starts_with("x"),
             names_to = "area_x",
             values_to = "area_id") |> 
filter(!is.na(area_id)) |> 
select(-area_x) |> 
mutate(across(.cols = c(countryside_stewardship, sfi, other_funding),
              ~replace_na(.x, "xxx")),
       grant_id = paste(countryside_stewardship,
                     sfi, 
                     other_funding,
                     sep = "; ") |> 
         str_remove_all("; xxx|\\r|\\n"),
       priority_id = as.integer(priority_id)) |> 
select(-c(countryside_stewardship, sfi, other_funding)) |> 
separate_longer_delim(grant_id, delim = "; ") |>
separate_longer_delim(stakeholder, delim = "; ") |>
separate_longer_delim(measure_type, delim = "; ") |>
    mutate(grant_id = if_else(grant_id == "xxx", NA_character_, grant_id)) |> 
distinct() 
}

make_area_measures_tbl <- function(area_measures_interim_tbl,
                                   areas_tbl,
                                   priorities_tbl,
                                   grants_tbl,
                                   area_schemes_condensed_tbl){

area_measures_interim_tbl |> 
left_join(areas_tbl, 
          by = join_by(area_id == area_id)) |> 
left_join(priorities_tbl, 
          by = join_by(priority_id == priority_id)) |> 
left_join(grants_tbl |> 
            select(grant_id,
            grant_name,
            grant_scheme,
            summary_wrapped,
            url),
          by = join_by(grant_id == grant_id)) |> 
  left_join(area_schemes_condensed_tbl,
          by = join_by(area_id == area_id)) 

}

area_measures_tbl <- make_measures_raw_tbl(
  sheets_list,
  measures_sheet_name = "measures_by_area",
  start_col_for_areas_index = 15) |> 
  make_area_measures_raw_tbl() |> 
  make_area_measures_interim_tbl() |> 
  make_area_measures_tbl(areas_tbl,
                         priorities_tbl,
                         grants_tbl,
                         area_schemes_condensed_tbl)

# check
# 
# area_measures_tbl |> 
#   filter(priority_id == 4, area_id == 7) |> 
#   group_by(measure) |> 
#   summarise(g = paste0(grant_id, collapse = "---")) |> 
#   view()


measures_tbl <- make_measures_raw_tbl(
  sheets_list,
  measures_sheet_name = "measures_by_area",
  start_col_for_areas_index = 15) |> 
  make_area_measures_raw_tbl() |> 
  transmute(
         measure,
         biodiversity_priority,
         core_supplementary,
         mapped_unmapped,
         measure_type,
         stakeholder,
         link_to_further_guidance,
         measure_id,
         priority_id = as.integer(priority_id)
         ) |> 
  glimpse()
  

# Species ----
# 
# Images

species_image_raw_tbl <- read_csv("data/images_out_dash_tbl.csv") |> select(usage_key,
         image_url = URLs) |> 
  mutate(usage_key = as.integer(usage_key)) 

species_image_metadata_tbl <- read_csv("data/portal_upload/images_out_tbl.csv") |> 
  select(-c(
    amended_file_name,
    full_file_path
  )) |> 
    mutate(usage_key = as.integer(usage_key)) |>
  glimpse()

species_image_tbl <- species_image_raw_tbl |> 
  inner_join(species_image_metadata_tbl,
             by = join_by(usage_key == usage_key)) |> glimpse()
  


make_priority_species_tbl <- function(sheets_list){
  sheets_list |> 
    pluck("priority_species") |> 
    rename(linnaean_name = linnaean)
}

get_gbif_tbl <- function(priority_species_tbl){
  # get the gbif definitive species data
  # If this fails ***********CHECK VPN***********
  test_status <- function(){
    
    status_code <- request("https://api.gbif.org/v1/species/match") |> 
      req_headers("Accept" = "application/json") |> 
      req_url_query(verbose = FALSE,
                    name = "Apus apus") |> 
      req_perform() |> 
      resp_status()
    
    if (status_code[1] == 200L) TRUE else FALSE
    
  }
  if ( isTRUE(test_status())) {
    priority_species_tbl |> 
      pull(linnaean_name) |> 
      name_backbone_checklist() |> 
      select(-rank, -confidence, -matchType, acceptedUsageKey) |> 
      mutate(gbif_species_url = glue("https://www.gbif.org/species/{usageKey}")) |> 
      clean_names()
  } else {
    print("Problem accessing GBIF - check VPN!")
  }
}

make_species_tbl <- function(priority_species_tbl, gbif_tbl, species_image_tbl){
  priority_species_tbl |> 
    select(-relevant_priorities, -link_to_further_guidance) |> 
    rename(common_name = species) |> 
    inner_join(gbif_tbl,
               by = join_by(linnaean_name == canonical_name)) |> 
    left_join(species_image_tbl |> 
                select(usage_key,
                       image_url,
                       license,
                       attribution),
              by = join_by(usage_key == usage_key))
}

priority_species_tbl <- make_priority_species_tbl(sheets_list)

gbif_tbl <- get_gbif_tbl(priority_species_tbl)


species_tbl <- make_species_tbl(priority_species_tbl, gbif_tbl, species_image_tbl) |> 
  glimpse()


species_priority_lookup_tbl <- 
priority_species_tbl |> 
select(species_id, relevant_priorities) |> 
separate_longer_delim(relevant_priorities, ", ") |> 
mutate(priority_id = as.integer(relevant_priorities),
       relevant_priorities = NULL) |> 
add_id() |> 
relocate(id, priority_id, species_id) |>
  glimpse()

species_area_lookup_tbl <- sheets_list |> 
pluck("species_by_area") |> 
slice(2:n()) |>
mutate(across(
  .cols = all_of(3:last_col()),
  ~ if_else(.x == "x", cur_column(),
            NA_character_))) |> 
rename(species_id = x1,
       species = identifier) |> 
filter(!is.na(species_id)) |>
pivot_longer(cols = -c(species_id, species)) |> 
mutate(area_id = str_sub(value, 2, 3) |>
         str_remove("_") |> 
         as.integer()) |>
filter(!is.na(value)) |> 
transmute(species_id = as.integer(species_id), area_id) |> 
arrange(area_id) |> 
add_id() |> 
relocate(id, species_id, area_id) |> 
  glimpse()

species_area_tbl <- species_area_lookup_tbl |> 
  left_join(areas_tbl |> select(area_id, area_name),
            by = join_by(area_id == area_id)) |> 
  left_join(species_tbl |> 
              select(species_id,
                     common_name,
                     scientific_name,
                     gbif_species_url,
                     image_url,
                     license,
                     attribution),
            by = join_by(species_id == species_id)) |> 
  glimpse()

species_priority_tbl <- species_priority_lookup_tbl |> 
  left_join(priorities_tbl |> 
              select(priority_id, biodiversity_priority),
            by = join_by(priority_id == priority_id)) |> 
  left_join(species_tbl |> 
              select(species_id,
                     common_name,
                     scientific_name,
                     gbif_species_url),
            by = join_by(species_id == species_id)) |> 
  glimpse()



# Write Data ----


tbl_list <- list(
"areas-tbl" = areas_tbl, 
"priorities-tbl" = priorities_tbl,
"measures-tbl" = measures_tbl,
"species-tbl" = species_tbl,
"area-measures-tbl" = area_measures_tbl,
"species-priority-tbl" = species_priority_tbl,
"species-area-tbl" = species_area_tbl,
"area-funding-schemes-tbl" = area_funding_schemes_tbl,
"grants-tbl" = grants_tbl
)

write_rds(tbl_list, glue("{upload_path}portal_tbl_list.rds"))

save_tbls(tbl_list, path = upload_path)

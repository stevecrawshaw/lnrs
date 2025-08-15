pacman::p_load(
  tidyverse,
  glue,
  janitor,
  readxl,
  writexl,
  rgbif,
  httr2,
  rvest,
  sf
)

# Source Data ----
path = "data/Priorities and Measures Master for portal_Copy for grants update May 25.xlsx"

sheets_vec <-
  readxl::excel_sheets(path)

make_list_from_sheets <- function(
  path = path,
  sheets_vec
) {
  # read all the sheets and store in a list with each element a sheet
  sheets_vec |>
    map(~ read_xlsx(path, .x)) |>
    set_names(make_clean_names(sheets_vec)) |>
    map(.f = clean_names)
}

sheets_list <- make_list_from_sheets(path, sheets_vec)

upload_path <- "data/portal_upload/"

# Utility Functions ----

linkchecker <- function(x) {
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
    error = function(e) {
      #message('Caught an error!')
      class(e)[1]
    },
    warning = function(w) {
      #message('Caught an warning!')
      class(w)[1]
    },
    finally = {
      #message('All done, quitting.')
    }
  )
}

save_tbls <- function(tbl_list, path = upload_path) {
  # take a named list and write csv's for the portal
  # and an excel file for introspection
  # csv - semicolon delims
  nms <- paste0(path, names(tbl_list), ".csv")
  walk2(tbl_list, nms, ~ write_csv2(.x, .y, na = ""))
  # excel for viewing
  write_xlsx(tbl_list, path = glue("{path}main_sheets.xlsx"))
}

add_id <- function(tbl, id_col = "id") {
  # helper to add autonumber integer
  tbl |>
    rownames_to_column(var = id_col) |>
    mutate(
      {{ id_col }} := as.integer(rlang::eval_bare(rlang::parse_expr(id_col)))
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


#    Grants ---- ###########################

# other funding codes
#
ofc_raw_tbl <- sheets_list |>
  pluck("other_funding_codes")

ofc_clean_tbl <- ofc_raw_tbl |>
  transmute(
    grant_id = code,
    grant_name = glue("{grant_id}: {fund_name}"),
    url = link
  )

# New countryside stewardship data
# CS has been replaced by CS - Higher Tier Actions and CS - Capital Items
# This code collects these data. Note it's not possible to apply yet (may 2025)
# But the urls point to generic guidance pages
csht_url <- "https://www.gov.uk/government/publications/countryside-stewardship-higher-tier-get-ready-to-apply/countryside-stewardship-higher-tier-actions"

csht_cap_url <- "https://www.gov.uk/government/publications/countryside-stewardship-higher-tier-get-ready-to-apply/countryside-stewardship-higher-tier-new-capital-items"


grant_scheme_from_url <- function(url) {
  url |>
    str_match("y/count.+$") |>
    str_remove("y/") |>
    str_split("-", simplify = TRUE) |>
    as.vector() |>
    map_chr(
      ~ substr(.x, 1, 1) |>
        str_to_upper()
    ) |>
    str_c(collapse = "")
}


make_csht_tbl <- function(url) {
  # return a table of grants and details from a url of the countryside
  # stewardship page. There are 2 here: Higher Tier and Capital Items
  gs = grant_scheme_from_url(url)

  url |>
    read_html(url) |>
    html_nodes("h3") |>
    html_attr("id") |>
    discard(is.na) |>
    tibble() |>
    set_names("action_id") |>
    mutate(grant_scheme = gs, base_url = url)
}

csht_tbl <- list(csht_url, csht_cap_url) |>
  map(make_csht_tbl) |>
  bind_rows()

cs_new_tbl <- csht_tbl |>
  mutate(
    grant_id = str_split_i(action_id, "-", i = 1) |> str_to_upper(),
    url = glue("{base_url}#{action_id}"),
    desc = map(
      action_id,
      ~ str_split(.x, "-", simplify = TRUE) |>
        discard_at(1) |>
        str_c(collapse = " ") |>
        str_to_sentence()
    ),
    grant_name = glue("{grant_id}: {desc}"),
    desc = NULL,
    action_id = NULL,
    base_url = NULL
  ) |>
  glimpse()


#     Consolidate grant data #######################################

grants_tbl <- bind_rows(cs_new_tbl, ofc_clean_tbl) |>
  mutate(
    grant_summary = glue(
      "Grant name: {grant_name}\n
   Grant scheme: {grant_scheme}\n
   Link: <a href={url} target=_blank>{url}</a>\n
   url: {url}"
    )
  ) |>
  add_id() |>
  glimpse()


# Habitat Creation and Restoration ----
#
parse_hab_sheet <- function(sheets_list, sheet_name) {
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
        NA_integer_
      )
    )) |>
    filter(identifier != "Habitat") |>
    pivot_longer(cols = -identifier, names_to = "col", values_to = "area_id") |>
    filter(!is.na(area_id)) |>
    select(-col) |>
    group_by(area_id) |>
    summarise(habitat = paste(identifier, collapse = ":\n")) |>
    arrange(area_id)
  names(out_tbl)[names(out_tbl) == "habitat"] <- sheet_name
  out_tbl
}

# 67 values as Avon Gorge has no habitat creation or management
hab_creation_tbl <- parse_hab_sheet(sheets_list, "bng_hab_creation") |>
  glimpse()

hab_mgt_tbl <- parse_hab_sheet(sheets_list, "bng_hab_mgt") |>
  glimpse()

# Create long tables for the source database

habitat_creation_long_tbl <- hab_creation_tbl |>
  separate_longer_delim(bng_hab_creation, delim = ":\n") |>
  set_names(c("area_id", "habitat")) |>
  glimpse()

habitat_management_long_tbl <- hab_mgt_tbl |>
  separate_longer_delim(bng_hab_mgt, delim = ":\n") |>
  set_names(c("area_id", "habitat")) |>
  glimpse()

habitat_tbl <- bind_rows(
  habitat_creation_long_tbl,
  habitat_management_long_tbl
) |>
  distinct(habitat) |>
  add_id("habitat_id") |>
  glimpse()

habitat_creation_area_lookup_tbl <- habitat_creation_long_tbl |>
  inner_join(habitat_tbl, by = join_by(habitat == habitat)) |>
  select(area_id, habitat_id) |>
  glimpse()


habitat_management_area_lookup_tbl <- habitat_management_long_tbl |>
  inner_join(habitat_tbl, by = join_by(habitat == habitat)) |>
  select(area_id, habitat_id) |>
  glimpse()

# Areas ----

parse_areas_tbl <- function(sheets_list) {
  # import and process the priority areas
  sheets_list |>
    pluck("areas_for_description") |>
    rename(
      area_id = identifier,
      area_name = title,
      area_description = brief_narrative,
      area_link = `links_to_further_info_guidance`
    ) |>
    mutate(
      area_id = as.integer(area_id),
      area_name = map_chr(area_name, ~ clean_text(.x))
    ) |>
    filter(!is.na(area_id))
}

interim_areas_tbl <- parse_areas_tbl(sheets_list) |>
  # remove trailing newlines
  mutate(across(
    .cols = c(area_link, local_funding_schemes),
    ~ str_remove_all(.x, "\\r|\\n")
  ))


make_area_funding_schemes_tbl <- function(interim_areas_tbl) {
  interim_areas_tbl |>
    select(area_id, area_name, local_funding_schemes) |>
    separate_longer_delim(cols = local_funding_schemes, delim = "; ") |>
    filter(!is.na(local_funding_schemes)) |>
    add_id() |>
    relocate(id, area_id, area_name, local_funding_schemes)
}

area_funding_schemes_tbl <- make_area_funding_schemes_tbl(interim_areas_tbl)


area_schemes_condensed_tbl <- area_funding_schemes_tbl |>
  group_by(area_id) |>
  summarise(
    local_funding_schemes = paste(local_funding_schemes, collapse = "\n")
  )

# just the shapes and id \ name - upload to the portal as geojson
areas_shape <- st_read("data/lnrs-sub-areas.fgb")

areas_tbl <- interim_areas_tbl |>
  mutate(area_link = str_replace_all(area_link, "; ", "\n")) |>
  select(-local_funding_schemes) |>
  left_join(hab_mgt_tbl, by = join_by(area_id == area_id)) |>
  left_join(hab_creation_tbl, by = join_by(area_id == area_id)) |>
  glimpse()


# areas_tbl |>
# write_csv("data/lnrs-areas-tbl.csv", na = "")

# Priorities ----

make_priorities_tbl <- function(
  sheets_list,
  bd_priorities_sheet_name = "statement_of_bd_priorities"
) {
  # read and process the priorities table.
  # amend area_count var if new areas are added

  priorities_raw_tbl <- sheets_list |>
    pluck("statement_of_bd_priorities")

  nms <- priorities_raw_tbl[1, ] |>
    as.character() |>
    make_clean_names()

  priorities_raw_tbl |>
    tail(-1) |>
    set_names(nms) |>
    mutate(
      priority_id = code |>
        as.integer(),
      code = NULL
    ) |>
    glimpse()
}

priorities_tbl <- make_priorities_tbl(sheets_list)

# Measures ----

make_measures_raw_tbl <- function(
  sheets_list,
  measures_sheet_name = "measures_by_area",
  start_col_for_areas_index = 16
) {
  # new columns added in latest version DO WE NEED OLD CS COLUMN?
  # do lots of renaming operations to create consistent names
  measures_raw_tbl <- sheets_list |>
    pluck(measures_sheet_name)
  # browser()
  (area_ids_raw <- names(measures_raw_tbl)[
    start_col_for_areas_index:dim(measures_raw_tbl)[2]
  ])

  # a vector of area id's with underscores and trailing numbers removed
  (area_ids_new <- if_else(
    str_detect(area_ids_raw, "_"),
    str_extract(area_ids_raw, "^(.*?)_"),
    area_ids_raw
  ) |>
    str_remove_all("_"))

  (non_area_names_old <- measures_raw_tbl[
    1,
    1:(start_col_for_areas_index - 1)
  ] |>
    as.character())

  (names(measures_raw_tbl)[
    1:start_col_for_areas_index - 1
  ] <- non_area_names_old |>
    make_clean_names())

  (names(measures_raw_tbl)[
    start_col_for_areas_index:dim(measures_raw_tbl)[2]
  ] <- area_ids_new)
  #names(measures_raw_tbl)
  measures_raw_tbl |>
    filter(!is.na(associated_priority_code))
}


make_area_measures_raw_tbl <- function(measures_raw_tbl) {
  measures_raw_tbl |>
    tail(-1) |>
    rename(priority_id = associated_priority_code) |>
    mutate(across(
      .cols = x1:last_col(),
      ~ if_else(.x == "x", cur_column(), NA_character_)
    )) |>
    rownames_to_column("measure_id") |>
    mutate(measure_id = as.integer(measure_id)) |>
    mutate(across(
      starts_with("x"),
      ~ str_remove(.x, "x") |>
        as.integer()
    ))
}

# inspect
make_measures_raw_tbl(
  sheets_list,
  measures_sheet_name = "measures_by_area",
  start_col_for_areas_index = 16
) |>
  make_area_measures_raw_tbl() |>
  view()


make_area_measures_interim_tbl <- function(area_measures_raw_tbl) {
  #browser()
  long_tbl <- area_measures_raw_tbl |>
    select(
      measure_id,
      starts_with("x"),
      priority_id,
      measure,
      other_priorities_delivered,
      core_supplementary,
      mapped_unmapped,
      measure_type,
      stakeholder,
      relevant_map_layer,
      link_to_further_guidance,
      #countryside_stewardship_old,
      higher_tier_countryside_stewardship_and_capital_items,
      #sfi,
      other_funding
    ) |>
    pivot_longer(
      cols = starts_with("x"),
      names_to = "area_x",
      values_to = "area_id"
    )

  filtered_long_tbl <- long_tbl |>
    filter(!is.na(area_id))

  grant_cleaned_tbl <- filtered_long_tbl |>
    select(-area_x) |>
    mutate(
      across(
        .cols = c(
          higher_tier_countryside_stewardship_and_capital_items,
          # sfi,
          other_funding
        ),
        ~ replace_na(.x, "xxx")
      ),
      grant_id = paste(
        # countryside_stewardship,
        # sfi,
        higher_tier_countryside_stewardship_and_capital_items,
        other_funding,
        sep = "; "
      ) |>
        str_remove_all("; xxx|\\r|\\n"),
      priority_id = as.integer(priority_id)
    ) |>
    select(
      -c(
        higher_tier_countryside_stewardship_and_capital_items,
        # sfi,
        other_funding
      )
    )

  sep_longer_tbl <- grant_cleaned_tbl |>
    separate_longer_delim(grant_id, delim = "; ") |>
    separate_longer_delim(stakeholder, delim = "; ") |>
    separate_longer_delim(measure_type, delim = "; ")

  final_grant_cleaned_tbl <- sep_longer_tbl |>
    mutate(grant_id = if_else(grant_id == "xxx", NA_character_, grant_id)) |>
    distinct()

  final_grant_cleaned_tbl
}


make_area_measures_tbl <- function(
  area_measures_interim_tbl,
  areas_tbl,
  priorities_tbl,
  grants_tbl,
  area_schemes_condensed_tbl
) {
  area_measures_interim_tbl |>
    left_join(areas_tbl, by = join_by(area_id == area_id)) |>
    left_join(priorities_tbl, by = join_by(priority_id == priority_id)) |>
    left_join(
      grants_tbl |>
        select(grant_id, grant_name, grant_scheme, grant_summary, url),
      by = join_by(grant_id == grant_id)
    ) |>
    left_join(area_schemes_condensed_tbl, by = join_by(area_id == area_id)) |>
    # remove any rows where there is a grant_id but no url
    # keep rows where there is no grant_id as some measures don't have grants
    filter(
      (!is.na(grant_id) &
        !is.na(url)) |
        is.na(grant_id)
    )
}

# get the shortened measures text from a separate sheet
measures_concise_tbl <- sheets_list |>
  pluck("measures_concise") |>
  select(measure_id, concise_measure) |>
  mutate(measure_id = as.integer(measure_id)) |>
  glimpse()

area_measures_tbl <- make_measures_raw_tbl(
  sheets_list,
  measures_sheet_name = "measures_by_area",
  start_col_for_areas_index = 16
) |>
  make_area_measures_raw_tbl() |>
  make_area_measures_interim_tbl() |>
  make_area_measures_tbl(
    areas_tbl,
    priorities_tbl,
    grants_tbl,
    area_schemes_condensed_tbl
  ) |>
  left_join(measures_concise_tbl, by = join_by(measure_id == measure_id))


# slimmed down version for app
area_measures_slim_tbl <- area_measures_tbl |>
  select(
    core_supplementary,
    measure_type,
    stakeholder,
    area_name,
    area_id,
    grant_id,
    priority_id,
    biodiversity_priority,
    measure,
    concise_measure,
    measure_id,
    link_to_further_guidance,
    grant_name,
    url
  ) |>
  glimpse()


# Species and measures and BENEFITS ---
#
#

benefits_tbl <- sheets_list |>
  pluck("references") |>
  head(1) |>
  select(1:8) |>
  as.character() |>
  unname() |>
  enframe(name = "benefit_id", value = "benefit") |>
  mutate(benefit_name = make_clean_names(benefit))

benefit_names <- benefits_tbl$benefit_name


# Get benefits and benefits lookup tbl ----
# to join onto measures_tbl to show co - benefits of measures

measures_benefits_ids_tbl <-
  sheets_list |>
  pluck(
    "measures_co_benefits"
  ) |>
  select(measure_id, any_of(benefit_names)) |>
  mutate(across(
    .cols = -measure_id,
    ~ if_else(
      .x %in% c("x", "X"),
      benefits_tbl$benefit_id[benefits_tbl$benefit_name == cur_column()],
      NA_integer_
    )
  )) |>
  glimpse()

measures_benefits_lookup_tbl <-
  measures_benefits_ids_tbl |>
  pivot_longer(
    cols = -measure_id,
    names_to = "benefit",
    values_to = "benefit_id"
  ) |>
  select(-benefit) |>
  filter(!is.na(benefit_id)) |>
  clean_names() |>
  glimpse()

measures_no_benefits_tbl <- make_measures_raw_tbl(
  sheets_list,
  measures_sheet_name = "measures_by_area",
  start_col_for_areas_index = 16
) |>
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
  # left_join(measures_benefits_grouped_tbl,
  #           by = join_by(measure_id == measure_id)) |>
  glimpse()

measures_benefits_tbl <-
  measures_benefits_lookup_tbl |>
  left_join(
    measures_no_benefits_tbl |>
      select(measure_id, measure),
    by = join_by(measure_id == measure_id)
  ) |>
  left_join(
    benefits_tbl |>
      select(benefit_id, benefit),
    by = join_by(benefit_id == benefit_id)
  ) |>
  glimpse()

benefits_tbl |> glimpse()

measures_benefits_grouped_tbl <-
  measures_benefits_tbl |>
  group_by(measure_id) |>
  summarise(benefits = paste(benefit, collapse = "\n")) |>
  glimpse()


# identify discrepancy between area_measures_tbl and measures_tbl
# measures_tbl|>
#   anti_join(area_measures_tbl |>
#               select(measure_id) |>
#               distinct(),
#             by = join_by(measure_id == measure_id)) |>
#   view()

# Species ----
#
# Images

species_image_raw_tbl <- read_csv("data/images_out_dash_tbl.csv") |>
  select(usage_key, image_url = URLs) |>
  mutate(usage_key = as.integer(usage_key))

species_image_metadata_tbl <- read_csv(
  "data/portal_upload/images_out_tbl.csv"
) |>
  select(
    -c(
      amended_file_name,
      full_file_path
    )
  ) |>
  mutate(usage_key = as.integer(usage_key)) |>
  glimpse()

species_image_tbl <- species_image_raw_tbl |>
  inner_join(
    species_image_metadata_tbl,
    by = join_by(usage_key == usage_key)
  ) |>
  glimpse()

make_priority_species_tbl <- function(sheets_list) {
  sheets_list |>
    pluck("priority_species") |>
    rename(linnaean_name = linnaean)
}

get_gbif_tbl <- function(priority_species_tbl) {
  # get the gbif definitive species data
  # If this fails ***********CHECK VPN***********
  test_status <- function() {
    status_code <- request("https://api.gbif.org/v1/species/match") |>
      req_headers("Accept" = "application/json") |>
      req_url_query(verbose = FALSE, name = "Apus apus") |>
      req_perform() |>
      resp_status()

    if (status_code[1] == 200L) TRUE else FALSE
  }
  if (isTRUE(test_status())) {
    priority_species_tbl |>
      pull(linnaean_name) |>
      name_backbone_checklist() |>
      select(-rank, -confidence, -matchType, acceptedUsageKey) |>
      mutate(
        gbif_species_url = glue("https://www.gbif.org/species/{usageKey}")
      ) |>
      clean_names()
  } else {
    print("Problem accessing GBIF - check VPN!")
  }
}

make_species_tbl <- function(
  priority_species_tbl,
  gbif_tbl,
  species_image_tbl
) {
  priority_species_tbl |>
    select(-relevant_priorities) |>
    rename(common_name = species, species_link = link_to_further_guidance) |>
    inner_join(gbif_tbl, by = join_by(linnaean_name == canonical_name)) |>
    left_join(
      species_image_tbl |>
        select(usage_key, image_url, license, attribution),
      by = join_by(usage_key == usage_key)
    )
}

priority_species_tbl <- make_priority_species_tbl(sheets_list)

gbif_tbl <- get_gbif_tbl(priority_species_tbl)

species_tbl <- make_species_tbl(
  priority_species_tbl,
  gbif_tbl,
  species_image_tbl
) |>
  glimpse()


species_priority_lookup_tbl <-
  priority_species_tbl |>
  select(species_id, relevant_priorities) |>
  separate_longer_delim(relevant_priorities, ", ") |>
  mutate(
    priority_id = as.integer(relevant_priorities),
    relevant_priorities = NULL
  ) |>
  add_id() |>
  relocate(id, priority_id, species_id) |>
  glimpse()

species_area_lookup_tbl <- sheets_list |>
  pluck("species_by_area") |>
  slice(2:n()) |>
  mutate(across(
    .cols = all_of(3:last_col()),
    ~ if_else(.x == "x", cur_column(), NA_character_)
  )) |>
  rename(species_id = x1, species = identifier) |>
  filter(!is.na(species_id)) |>
  pivot_longer(cols = -c(species_id, species)) |>
  mutate(
    area_id = str_sub(value, 2, 3) |>
      str_remove("_") |>
      as.integer()
  ) |>
  filter(!is.na(value)) |>
  transmute(species_id = as.integer(species_id), area_id) |>
  arrange(area_id) |>
  add_id() |>
  relocate(id, species_id, area_id) |>
  glimpse()

species_area_tbl <- species_area_lookup_tbl |>
  left_join(
    areas_tbl |> select(area_id, area_name),
    by = join_by(area_id == area_id)
  ) |>
  left_join(
    species_tbl |>
      select(
        species_id,
        common_name,
        scientific_name,
        gbif_species_url,
        species_link,
        image_url,
        license,
        attribution
      ),
    by = join_by(species_id == species_id)
  ) |>
  glimpse()

species_priority_tbl <- species_priority_lookup_tbl |>
  left_join(
    priorities_tbl |>
      select(priority_id, biodiversity_priority),
    by = join_by(priority_id == priority_id)
  ) |>
  left_join(
    species_tbl |>
      select(
        species_id,
        common_name,
        scientific_name,
        gbif_species_url,
        species_link
      ),
    by = join_by(species_id == species_id)
  ) |>
  glimpse()


# Get species measures lookup tbl ----

species_names_tbl <- species_tbl |>
  transmute(
    common_name,
    clean_name = make_clean_names(common_name),
    species_id
  ) |>
  glimpse()

#
species_measures_ids_tbl <-
  sheets_list |>
  pluck("measures_species") |>
  select(measure_id, everything(), -c(priority, measure)) |>
  mutate(across(
    .cols = -measure_id,
    ~ if_else(
      .x %in% c("x", "X"),
      species_names_tbl$species_id[
        species_names_tbl$clean_name == cur_column()
      ],
      NA_integer_
    )
  )) |>
  glimpse()

species_measures_lookup_tbl <-
  species_measures_ids_tbl |>
  pivot_longer(
    cols = -measure_id,
    names_to = "species",
    values_to = "species_id"
  ) |>
  select(-species) |>
  filter(!is.na(species_id)) |>
  clean_names() |>
  glimpse()

species_measures_tbl <-
  species_measures_lookup_tbl |>
  left_join(species_tbl, by = join_by(species_id == species_id)) |>
  group_by(measure_id) |>
  summarise(species = paste(common_name, collapse = "\n")) |>
  glimpse()


measures_tbl <- measures_no_benefits_tbl |>
  inner_join(
    measures_benefits_grouped_tbl, # add benefits col
    by = join_by(measure_id == measure_id)
  ) |>
  left_join(
    species_measures_tbl, # add species col
    by = join_by(measure_id == measure_id)
  ) |>
  left_join(measures_concise_tbl, by = join_by(measure_id == measure_id)) |>
  glimpse()


# measures_concise_tbl <- read_lines(file = "data/measures_concise.txt") |>
#   enframe() |>
#   mutate(measure = str_remove_all(value, "\\r|\\n|\"")) |>
#   separate_wider_delim(cols = measure,
#                        delim = ";",
#                        names = c("measure_id", "concise_measure")) |>
#   mutate(measure_id = as.integer(measure_id),
#          name = NULL,
#          value = NULL) |>
#   glimpse()
#
# measures_concise_tbl |>
#   write_xlsx("data/measures_concise.xlsx")
#
area_measures_tbl |>
  filter(area_id < 5) |>
  write_csv("data/gemini-area-measures-tbl.csv", na = "")


# Write Data ----

tbl_list <- list(
  "areas-tbl" = areas_tbl,
  "priorities-tbl" = priorities_tbl,
  "measures-tbl" = measures_tbl,
  "species-tbl" = species_tbl,
  "area-measures-tbl" = area_measures_tbl,
  "lnrs-measures-priorities-grants-slim-tbl" = area_measures_slim_tbl,
  "species-priority-tbl" = species_priority_tbl,
  "species-area-tbl" = species_area_tbl,
  "area-funding-schemes-tbl" = area_funding_schemes_tbl,
  "grants-tbl" = grants_tbl,
  "habitat-tbl" = habitat_tbl,
  "habitat-creation-area-lookup-tbl" = habitat_creation_area_lookup_tbl,
  "habitat-management-area-lookup-tbl" = habitat_management_area_lookup_tbl,
  "measures_benefits_lookup_tbl" = measures_benefits_lookup_tbl,
  "benefits_tbl" = benefits_tbl,
  "species_measures_lookup_tbl" = species_measures_lookup_tbl
)

write_rds(tbl_list, glue("{upload_path}portal_tbl_list.rds"))

save_tbls(tbl_list, path = upload_path)

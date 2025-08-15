pacman::p_load(tidyverse, glue, janitor, httr2, magick, jsonlite, fs, fuzzyjoin)

# Get the species data

species_url <- "https://opendata.westofengland-ca.gov.uk/api/explore/v2.1/catalog/datasets/lnrs-species-tbl/exports/csv?lang=en&timezone=Europe%2FLondon&use_labels=false&delimiter=%2C"

# just region
geometry <- "POLYGON((-3.01846 51.16792,-2.60807 51.15443,-2.38747 51.21087,-2.33974 51.24099,-2.24426 51.30356,-2.35267 51.47827,-2.3553 51.63261,-2.57787 51.6515,-2.93306 51.44899,-3.07172 51.31774,-3.01846 51.16792))"

# whole of UK
geometry_uk <- "POLYGON((-7.15859 56.32607,-5.63629 54.95911,-5.10815 54.05816,-5.79163 52.00773,-6.1955 49.49128,-1.84609 50.2369,1.07423 50.45437,2.13051 51.60385,2.47225 53.21935,-0.6966 55.36299,-1.78396 56.57461,-1.34901 57.94157,-2.28103 58.50078,-2.5917 59.68133,-5.04601 59.55706,-8.33914 57.7241,-7.9042 56.66781,-7.15859 56.32607))"

# Europe
#
geometry_eu <- "POLYGON((-14.5671 53.18524,-16.93829 47.49439,-3.77818 34.68996,26.21737 35.63844,45.89824 49.15422,43.52705 71.56196,28.94424 75.47443,-14.5671 53.18524))"

# example gallery url
ex_url <- "https://www.gbif.org/occurrence/gallery?taxon_key=1912325&geometry=POLYGON((-3.01846%2051.16792,-2.60807%2051.15443,-2.38747%2051.21087,-2.33974%2051.24099,-2.24426%2051.30356,-2.35267%2051.47827,-2.3553%2051.63261,-2.57787%2051.6515,-2.93306%2051.44899,-3.07172%2051.31774,-3.01846%2051.16792))"
#
species_tbl <- read_csv(species_url)

taxon_keys <- species_tbl$usage_key
# Define parameters

# function to get media tables for a given taxon
get_media <- function(
  taxon_key,
  geometry,
  limit = 300,
  url = "https://api.gbif.org/v1/occurrence/search"
) {
  # For a given taxon key (usage_key) and geometry, get media (images and videos) from GBIF
  # Make GET request with parameters
  response <-
    request(url) |>
    req_url_query(
      taxonKey = taxon_key,
      geometry = geometry,
      limit = limit # Adjust the limit as needed; the maximum is typically 300 per request
    ) |>
    req_perform()

  # Check if the request was successful
  if (response$status_code == 200) {
    # Parse the JSON content

    data <- resp_body_json(response)
    # Extract occurrence IDs
    results <- data$results
    # Display the occurrence IDs
  } else {
    # Handle errors
    print(paste("Request failed with status ", response$status_code))
  }

  rights_holder <- results$rightsHolder

  map(results, ~ pluck(.x, "media")) |>
    bind_rows() |>
    mutate(rights_holder = rights_holder)
}

# for the WECA region
sp_media_list <- map(
  taxon_keys,
  ~ get_media(.x, geometry = geometry, limit = 30)
) |>
  set_names(species_tbl$usage_key)


make_media_tbl <- function(media_list) {
  media_list |>
    bind_rows(.id = "usage_key") |>
    mutate(usage_key = as.integer(usage_key))
}

sp_media_tbl <- sp_media_list |>
  make_media_tbl()

make_file_tbl <- function(media_tbl) {
  media_tbl |>
    filter(format == "image/jpeg", !is.na(identifier)) |>
    inner_join(
      species_tbl |> select(common_name, usage_key),
      by = "usage_key"
    ) |>
    mutate(
      license_short = str_extract(license, "by.*/{1}$"),
      across(
        .cols = c(license_short, common_name, rightsHolder),
        ~ map_chr(.x, make_clean_names)
      ),
      filepath = glue(
        "data/species_images/{common_name}__{usage_key}/{common_name}__{license_short}__{rightsHolder}__.jpg"
      )
    )
}

sp_media_file_tbl <- sp_media_tbl |>
  make_file_tbl() |>
  glimpse()

# make folder names for images
folder_names <- species_tbl |>
  transmute(
    folder = glue(
      "data/species_images/{make_clean_names(common_name)}__{usage_key}"
    )
  ) |>
  pull()

# Create folders

walk(folder_names, ~ dir_create(.x, mode = 777))

# Download images
# download.file("https://observation.org/photos/91513771.jpg",
#               destfile = "data/species_images/test.jpg", method = "curl")

dl_files <- function(media_file_tbl) {
  walk2(
    media_file_tbl$identifier,
    media_file_tbl$filepath,
    ~ download.file(.x, .y, method = "curl")
  )
}

# Widen the geographical search for those items where we don't have any images

# Identify folders with no images
#
species_top_folder <- "data/species_images"

species_folders <- fs::dir_info(species_top_folder)

empty_folders <- species_folders |>
  filter(blocks == 0) |>
  pull(path)

empty_folders
# extract taxon keys for where we want images outside weca area
uk_taxon_keys <- empty_folders |>
  as.character() |>
  str_extract("[0-9]{7}$") |>
  as.integer()

uk_taxon_keys

species_missing <- empty_folders |>
  as.character() |>
  str_remove("data/species_images/")
species_missing

# get the images
uk_media_list <- map(
  uk_taxon_keys,
  ~ get_media(.x, geometry = geometry_uk, limit = 30)
) |>
  set_names(uk_taxon_keys)

uk_file_media_tbl <- uk_media_list |>
  make_media_tbl() |>
  make_file_tbl() |>
  dl_files()

needed_tbl <- c(
  "A lepiota",
  "Barbastelle",
  "Bechstein's",
  "Big Blue Pinkgill",
  "Blushing waxcap",
  "Bristol Whitebeam",
  "(Chalkhill Blue)",
  "Common Dormouse",
  "(European Eel)",
  "European Beaver",
  "Gadwall",
  "(Grayling)",
  "(Hedgehog)",
  "(lapwing)",
  "Lesser Horseshoe Bat",
  "Mistletoe Marble",
  "Shoveler",
  "(olive Earthtongue)",
  "(Redshank)",
  "Round leaved whitebeam",
  "Satan's Bolete",
  "Service tree",
  "(Shelduck)",
  "Silky Wave",
  "Small Blue",
  "(Swallow)",
  "(Swift)",
  "(Water Vole)",
  "Western Wood Vase Hoverfly",
  "(White clawed Crayfish)",
  "Wilmott's whitebeam"
) |>
  make_clean_names() |>
  enframe()

missing_tbl <- species_tbl |>
  select(usage_key, common_name) |>
  mutate(match_name = make_clean_names(common_name)) |>
  stringdist_join(
    needed_tbl,
    by = join_by(match_name == value),
    mode = 'inner',
    method = 'lv',
    max_dist = 3,
    ignore_case = TRUE,
    distance_col = 'distance'
  ) |>

  glimpse()

missing_taxonkeys_manual <- c(2432582L, 2432427L, 2481714L, 2498089L, 2481714L)

missing_taxonkeys_matched <- missing_tbl$usage_key |> as.integer()

missing_taxon_keys <- c(missing_taxonkeys_matched, missing_taxonkeys_manual) |>
  as.character()


eu_media_list <- map(
  missing_taxon_keys,
  ~ get_media(.x, geometry = geometry_eu, limit = 30)
) |>
  set_names(missing_taxon_keys)

eu_file_media_tbl <- eu_media_list |>
  make_media_tbl() |>
  make_file_tbl() |>
  dl_files()

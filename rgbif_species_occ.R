pacman::p_load(tidyverse,
               glue,
               janitor,
               fs,
               rgbif,
               config,
               yaml
               )

gbif_creds <- config::get(file = "../config.yml", config = "gbif")

# specified by stuart
#species_tbl <- read_csv2("data/species.csv")
ods_species_path <- "https://opendata.westofengland-ca.gov.uk/api/explore/v2.1/catalog/datasets/lnrs-species-tbl/exports/csv?lang=en&timezone=Europe%2FLondon&use_labels=false&delimiter=%2C"
species_tbl <- read_csv(ods_species_path)


#https://geopick.gbif.org/
bounds <- "POLYGON (-3.065186 51.33576, -2.842712 51.486514, -2.58728 51.633362, -2.183533 51.551459, -2.246704 51.299711, -2.584534 51.218927, -3.043213 51.284253, -3.065186 51.33576)"

bounds <- "POLYGON ((-3.065186 51.33576, -3.043213 51.284253, -2.584534 51.218927, -2.246704 51.299711, -2.183533 51.551459, -2.58728 51.633362, -2.842712 51.486514, -3.065186 51.33576))"
# taxonKeys
keys <- species_tbl$usage_key
# set up the preds
years <- pred_in("year", 2019:2024)
area <- pred_within(bounds) #west of england
taxons <- pred_in("taxonKey", keys)

payload <- pred_and(years, area, taxons)
# get the ID
dl_key <- occ_download(payload,
                       user = gbif_creds$user,
                       pwd = gbif_creds$pwd, 
                       email = gbif_creds$email)

dl_id <- yaml.load(dl_key)


dl_key
# see how long..
occ_download_wait(dl_id)
# download
species_gbif_tbl <- occ_download_get(dl_id, overwrite = TRUE) %>% 
    occ_download_import()
# inspect
species_gbif_tbl %>% 
  glimpse()

species_gbif_tbl$usage
# clean it
ods_species_tbl <- species_gbif_tbl %>% 
  select(modified,
         license,
         rightsHolder,
         institutionCode,
         sex, 
         lifeStage, 
         eventDate, year, verbatimLocality, latitude = decimalLatitude,
         longitude = decimalLongitude, scientificName, kingdom, phylum, class,
         order, family, genericName, lastInterpreted, taxonKey, species, level3Name)

unique(ods_species_tbl$license)

# write
ods_species_tbl %>% 
  clean_names() %>% 
  write.csv2("data/lnrs_species_occ_gbif_tbl.csv", row.names = FALSE, na = "")

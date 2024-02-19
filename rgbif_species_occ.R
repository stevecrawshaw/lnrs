pacman::p_load(tidyverse,
               glue,
               janitor,
               fs,
               rgbif,
               config
               )

gbif_creds <- config::get(file = "../config.yml", config = "gbif")

# specified by stuart
species_tbl <- read_csv2("data/species.csv")
#https://geopick.gbif.org/
bounds <- "POLYGON ((-3.065186 51.33576, -2.842712 51.486514, -2.58728 51.633362, -2.183533 51.551459, -2.246704 51.299711, -2.584534 51.218927, -3.043213 51.284253, -3.065186 51.33576))"
# taxonKeys
keys <- species_tbl$usageKey
# set up the preds
years <- pred_in("year", 2019:2023)
area <- pred_within(bounds) #west of england
taxons <- pred_in("taxonKey", keys)

payload <- pred_and(years, area, taxons)
# get the ID
dl_key <- occ_download(payload,
                       user = gbif_creds$user,
                       pwd = gbif_creds$pwd, 
                       email = gbif_creds$email)

dl_key
# see how long..
occ_download_wait('0018392-231120084113126')
# download
species_gbif_tbl <- occ_download_get('0018392-231120084113126') %>% 
    occ_download_import()
# inspect
species_gbif_tbl %>% 
  glimpse()
# clean it
ods_species_tbl <- species_gbif_tbl %>% 
  select(modified,
         rightsHolder,
         institutionCode,
         sex, 
         lifeStage, 
         eventDate, year, verbatimLocality, latitude = decimalLatitude,
         longitude = decimalLongitude, scientificName, kingdom, phylum, class,
         order, family, genericName, lastInterpreted, taxonKey, species, level3Name)
# write
ods_species_tbl %>% 
  clean_names() %>% 
  write.csv2("data/ods_species_tbl.csv", row.names = FALSE)

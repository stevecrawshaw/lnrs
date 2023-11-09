pacman::p_load(tidyverse,
               rgbif,
               glue,
               janitor,
               sf)

# get the Wildlife index data from CSV
# this was converted from pdf by chatgpt
bwi_tbl <- read_csv('data/bristol_wildlife_index_species.csv', 
                    name_repair = make_clean_names)
# reconcile the scientific names with GBIF's database
nbc_tbl <- name_backbone_checklist(bwi_tbl$linnaean_name, verbose = FALSE)

# run some checks where the matches weren't exact
nbc_match <- nbc_tbl %>% 
  add_column(bwi_tbl$common_name,
             bwi_tbl$linnaean_name,
             .after = "canonicalName") %>% 
  select(1:9, verbatim_name) %>%
  filter(matchType != "EXACT") %>% 
  view()

nbc_tbl %>% 
  filter(usageKey < 1300)

# get the taxon keys of interest for BWI
bwi_usagekey <- nbc_tbl$usageKey
# read in the WoE polygon
woe <- st_read(dsn = "C:\\Users\\steve.crawshaw\\OneDrive - West Of England Combined Authority\\Documents\\qgis\\gis_files\\WOE MM CLIPPED.TAB", crs = 27700) %>% 
  st_transform(crs = 4326)

plot(woe)

# we have to generalise this to a bounding box because the WoE polygon
# is complex and violates the rules for the order of points GBIF expects
# from a polygon (i.e. clockwise)

# can clip with the WoE poly later for precision

wopoly <- st_bbox(woe) %>% 
  st_as_sfc() %>% 
  st_as_text()

# The API call below should send an email when its ready
# My credentials are in .Environ
occ_download(
  pred_in("taxonKey", bwi_usagekey), 
  pred("hasCoordinate", TRUE),
  pred_within(wopoly),
  format = "SIMPLE_CSV"
)
#  https://doi.org/10.15468/dl.e6bbqg
occ_download_wait('0256176-230224095556074')

d <- occ_download_get('0256176-230224095556074') %>%
  occ_download_import()

saveRDS(d, file = "data/bwi_woe.rds")

Sys.getenv()

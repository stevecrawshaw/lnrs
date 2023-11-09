pacman::p_load(fastverse,
               tidyverse,
               glue,
               janitor,
               sf)
# https://records.nbnatlas.org/explore/your-area?q=*:*&lat=51.43072&lon=-2.562313&radius=10&fq=(geospatial_kosher:true

# not for the whole of WOE - 10KM radius bristol

raw_nbda_tbl <- read_csv('data/nbda/records-2023-06-28.csv')

raw_nbda_tbl %>% 
  filter(!is.na(collectionCode),
         `year processed` >= 1995) %>% 
  group_by(collectionCode, `year processed`) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  view()

  view()
  filter(institutionCode == "BTO",
         `year processed` == 2019) %>% 
  view()
  
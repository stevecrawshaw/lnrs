pacman::p_load(tidyverse,
               sf,
               glue,
               janitor,
               fs,
               rlist,
               tidyxl)

oep_species_raw <- read_csv("data/ea_oep_species.csv")


taxa <- c("Mammals", "Vascular Plants", "Birds", "Bumblebees", "Fish", "Freshwater invertebrates", "Lepidoptera (Moths)", "Lepidoptera (Butterfly)")


oep_species_tbl <- oep_species_raw %>% 
  filter(!scientific_name %in% taxa) %>% 
  write_csv("data/ea_oep_species_clean.csv")

woe_species_tbl <- read_csv2("data/portal_upload/species_tbl.csv")

oep_woe_species_tbl <- oep_species_tbl %>% 
  inner_join(woe_species_tbl, by = join_by(scientific_name == canonicalName))





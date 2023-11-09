pacman::p_load(
  fastverse,
  tidyverse,
  rgbif,
  glue,
  janitor,
  sf,
  DataExplorer, 
  paletteer, 
  scales)

source("../airquality_GIT/gg_themes.R")
bwi_woe_raw_tbl <- read_rds('data/bwi_woe.rds')

woe <- st_read(dsn = "C:\\Users\\steve.crawshaw\\OneDrive - West Of England Combined Authority\\Documents\\qgis\\gis_files\\WOE MM CLIPPED.TAB", crs = 27700) %>% 
  st_transform(crs = 4326)

woe_bwi_sf <- bwi_woe_raw_tbl %>%
  filter(year >= 2000) %>% 
    st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) %>% 
    st_intersection(woe)

write_rds(woe_bwi_sf, file = 'data/woe_bwi_sf.rds')

woe_bwi_sf <- read_rds(file = 'data/woe_bwi_sf.rds')


woe_slim_sf <- woe_bwi_sf %>% 
  slt(kingdom,
      phylum,
      class,
      order,
      family, 
      genus,
      species,
      speciesKey,
      taxonRank,
      taxonKey,
      scientificName,
      locality,
      occurrenceStatus,
      eventDate,
      year,
      institutionCode,
      recordedBy,
      lastInterpreted) %>% 
  tfm(institutionCode = if_else(institutionCode == "",
                                "Individual",
                                str_replace_all(institutionCode,
                                                pattern = " ",
                                                replacement = "\n")))


woe_bwi_sf %>% 
  rm()


woe_slim_sf %>% 
  filter(year >= 2020) %>% 
  st_drop_geometry() %>% 
  create_report()

bwi_woe_plot <- woe_slim_sf %>% 
  select(year, institutionCode) %>% 
  filter(between(year, 2018, 2022)) %>% 
  st_drop_geometry() %>% 
  # group_by(year, institutionCode) %>% 
  summarise(count = n(), .by = c(year, institutionCode))  %>% 
  filter(count > 500) %>%
  ggplot(aes(x = year, y = count, colour = institutionCode)) +
    geom_line(linewidth = 2) +
  # guides(colour = guide_legend(nrow = 2)) +
  # facet_wrap(~ order) +
  scale_color_paletteer_d(`"tidyquant::tq_light"`) +
  theme_web_classic() +
  theme(plot.subtitle = element_text(vjust = 6),
        axis.text.x = element_text(size = 15)) +
  labs(title = "GBIF Observations of Bristol Wildlife Index Species",
       subtitle = "West of England (inc N. Somerset). Counts < 500 removed",
       colour = " Institution",
       x = "Year",
       y = "Count",
       caption = "GBIF.org (22 May 2023) GBIF Occurrence Download https://doi.org/10.15468/dl.e6bbqg")

bwi_woe_plot %>% 
  ggsave('plots/gbif_woe_bwi_plot.png', .)

bwi_tbl <- read_csv('data/bristol_wildlife_index_species.csv', 
                    name_repair = make_clean_names)
# reconcile the scientific names with GBIF's database
nbc_tbl <- rgbif::name_backbone_checklist(bwi_tbl$linnaean_name, verbose = FALSE)

nbc_tbl %>%
  summarise(prop = n() / nrow(.), .by = kingdom)
  
woe_slim_sf %>% 
  st_drop_geometry() %>% 
  summarise(prop = n() / nrow(.), .by = kingdom)

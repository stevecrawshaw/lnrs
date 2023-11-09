pacman::p_load(tidyverse,
               glue,
               janitor,
               sf,
               fs)

folder <- 'C:\\Users\\steve.crawshaw\\OneDrive - West Of England Combined Authority\\Documents\\qgis\\gis_files\\Designations\\'

files <- fs::dir_ls(folder)

snci <- st_read(glue("{folder}SNCIs West of England.shp"))

create_report(snci %>% st_drop_geometry())

snci_clean_tbl <- snci %>% 
  st_drop_geometry() %>% 
  # filter(!is.na(LAST_SURVE)) %>% 
  select(NAME, LAST_SURVE) %>% 
  as_tibble() %>%
  mutate(clean_year = str_extract(LAST_SURVE, '\\b\\d{4}\\b$')) %>% 
  filter(!is.na(clean_year))

snci_plot <- snci_clean_tbl %>% 
  ggplot() +
  geom_bar(aes(clean_year), fill = weca_core_colours[2]) +
  theme_minimal(base_size = 20) +
  theme(axis.text.x = element_text(angle = 45, size = 10),
        panel.grid = element_blank()) +
  labs(x = "Year (where known)",
       y = "Count",
       title = "Years of SNCI Surveys",
       subtitle = "Distribution of last year surveyed",
       caption = glue("{nrow(snci_clean_tbl)} valid dates out of {nrow(snci)}"))


ggsave('plots/snci_survey_plot.png', plot = snci_plot, bg = 'white', device = 'png')



sssi <- st_read(glue("{folder}SSSI.TAB"))
sssi %>% view()
sna <- st_read(glue("{folder}Strategic Nature Areas.TAB"))

woe_lnr <- st_read(glue('{folder}WOE_Local_Nature_Reserves.shp'))

spa <- st_read(glue('{folder}SpecialProtectionAreas.TAB'))
sac <- st_read(glue('{folder}SpecialAreasOfConservation.TAB'))
phi <-  st_read(glue('{folder}PriorityHabitatsInventory.TAB'))

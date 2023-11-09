pacman::p_load(tidyverse,
               glue,
               janitor,
               fs,
               sf,
               readxl,
               patchwork)
rm(list = ls())

# Load SSSI data ----

#https://designatedsites.naturalengland.org.uk/Reserved.ReportViewerWebControl.axd?Culture=2057&CultureOverrides=True&UICulture=2057&UICultureOverrides=True&ReportStack=1&ControlID=026fe57579b64ea1b0f4107eb8111112&Mode=true&OpType=Export&FileName=SiteUnitConditionAssessment&ContentDisposition=OnlyHtmlInline&Format=EXCELOPENXML

weca_core_colours <- jsonlite::read_json('https://raw.githubusercontent.com/westofengland-ca/weca_templates/main/Power_Platform/PowerBI/WECABITheme2021.json')$dataColors %>%
  unlist()

sssi <- read_xlsx('data/SiteUnitConditionAssessment.xlsx',
                     sheet = 'SiteUnitConditionAssessment',
                     range = 'A3:K409') %>%
  clean_names()

# Load SNCI data ----

folder <-
  'C:\\Users\\steve.crawshaw\\OneDrive - West Of England Combined Authority\\Documents\\qgis\\gis_files\\Designations\\'

snci <- st_read(glue("{folder}SNCIs West of England.shp"))

# SNCI functions ----

clean.snci.tbl <- function(snci) {
  snci %>%
    st_drop_geometry() %>%
    select(NAME, LAST_SURVE) %>%
    as_tibble() %>%
    mutate(clean_year = str_extract(LAST_SURVE, '\\b\\d{4}\\b$')) %>%
    filter(!is.na(clean_year)) %>%
    return()
  
}

plot.snci <- function(snci_clean_tbl){
snci_clean_tbl %>%
  ggplot() +
  geom_bar(aes(clean_year), fill = weca_core_colours[2]) +
  theme_minimal(base_size = 20) +
  theme(axis.text.x = element_text(angle = 45, size = 10),
        panel.grid = element_blank()) +
  labs(
    x = "Year (where known)",
    y = "Count",
    title = "SNCI Condition Surveys",
    subtitle = glue("West of England \n{nrow(snci_clean_tbl)} valid dates out of {nrow(snci)}")
  )
}

# SSSI functions ----
# clean.sssi.tbl <- function(sssi_units_tbl) {
#   sssi_units_tbl %>%
#     mutate(
#       nnr_overlapping_area_ha = as.numeric(nnr_overlapping_area_ha),
#       date = dmy(latest_assessment_date),
#       year = year(date),
#       x4 = NULL
#     ) %>%
#     return()
# }
clean.sssi.tbl <- function(sssi) {
  sssi %>%
    mutate(
      site_bin = if_all(responsible_officer:comment,
                        .fns = ~ is.na(.x)),
      site = if_else(site_bin, main_habitat, NA_character_),
      x4 = NULL,
      date = dmy(latest_assessment_date),
      year = year(date),
      nnr_overlap_area_ha = nnr_overlap_area_ha %>% as.numeric()
    ) %>%
    fill(site, .direction = 'down') %>%
    filter(!site_bin) %>%
    select(-site_bin) %>%
    return()
}

plot.sssi <- function(clean_sssi_tbl) {
  year_count_tbl <- clean_sssi_tbl %>%
    summarise(count = n(), .by = year)
  
  year_count_tbl %>%
    ggplot() +
    geom_col(aes(x = year, y = count),
             fill = weca_core_colours[2]) +
    theme_minimal(base_size = 20) +
    labs(
      title = "SSSI Unit Condition Surveys",
      subtitle = glue(
        "West of England: {sum(year_count_tbl$count)} sites"
      ),
      x = "Year",
      y = "Count"
    ) +
    theme(axis.text.x = element_text(angle = 45, size = 10),
          panel.grid = element_blank()) %>%
    return()
  
}



sssi_hist_plot <- sssi %>%
  clean.sssi.tbl() %>%
  plot.sssi()

sssi_hist_plot

snci_hist_plot <- snci %>% 
  clean.snci.tbl() %>% 
  plot.snci()

snci_hist_plot


charts <- sssi_hist_plot + snci_hist_plot

charts


ggsave(
  'plots/snci_sssi_hist_plots.png',
  plot = charts,
  bg = 'white',
  device = 'png', width = 11, height = 6, units = "in"
)


text <-
  'AGRICULTURE - AGRICULTURE - OTHER,FRESHWATER - INAPPROPRIATE WATER LEVELS,FRESHWATER POLLUTION - WATER POLLUTION - AGRICULTURAL SOURCES,FRESHWATER POLLUTION - WATER POLLUTION - DISCHARGE,FRESHWATER POLLUTION - WATER POLLUTION - OTHER DISCHARGES,FRESHWATER POLLUTION - WATER POLLUTION - UNKNOWN,FRESHWATER POLLUTION - WATER POLLUTION - URBAN AND/OR ROAD SOURCES,'

split <- (strsplit(text, ",(?![,\\s])", perl = TRUE))


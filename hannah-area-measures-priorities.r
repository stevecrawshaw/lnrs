pacman::p_load(tidyverse, janitor, glue)

amp_tbl <- read_csv(
  'https://opendata.westofengland-ca.gov.uk/api/explore/v2.1/catalog/datasets/apmg_slim_ods/exports/csv?lang=en&timezone=Europe%2FLondon&use_labels=false&delimiter=%2C'
)

amp_tbl |>
  distinct(measure, area_name, biodiversity_priority) |>
  arrange(measure, area_name) |>
  write_excel_csv("data/areas_measures_priorities_hannah.csv")

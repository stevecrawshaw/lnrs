pacman::p_load(fastverse,
               tidyverse,
               glue,
               janitor,
               sf)

# https://climatedataportal.metoffice.gov.uk/datasets/TheMetOffice::monthly-temperature-observations-1991-2020/explore?location=51.320446%2C-2.517448%2C9.85

temp_obs_sf <- st_read('data/Monthly_Temperature_Observations_1991-2020.geojson')

pacman::p_load(tidyverse,
               janitor,
               glue,
               fs,
               gt,
               colorDF)


get_lnrs_data_raw <- function(){
  tibble(
             Theme = c("Wildlife abundance/distribution",NA,"Quality of protected sites network",NA,
                       "Biodiversity Net Gain",NA,"Land managed for nature",NA,
                       "Trees and Woodland",NA,"Habitat: extent",NA,
                       "Water Quality and Quantity",NA,"GI Provision",NA,
                       "Accessible Green Space",NA,"Green Jobs",NA,
                       "Sustainable food growing",NA),
  `Data:.existing` = c("Avon Ornothological Club bird surveys","BRERC Distribution data",
                       "SSSI Status for region","Woodland ecological condition",NA,NA,
                       "Land in CS/ELM Scheme",
                       "Area of designated sites and nature reserves","Canopy Cover (Forestry Research Tool)",
                       "Trees for Climate Monitoring","Priority habitat mapping",
                       "'Living England' Habitat probability mapping",
                       "Sub-catchment water quality","Fish barriers",
                       "Investment in GI Projects","Number of GI Projects funded",
                       "Access to Natural Green Space (ANGST)","Area of OS open green space",NA,NA,
                       "Land in CS/ELM Scheme","Number of allotments"),
    `Data:.future` = c("Wildlife index",NA,NA,NA,
                       "Net Gain Metric",NA,NA,NA,"Other remote sensing",NA,
                       NA,NA,"eDNA Sampling?","Real-time monitoring",NA,NA,
                       NA,NA,"Number of jobs",NA,"Soil Health",
                       "People growing own food")
)
}

lnrs_data_raw <- get_lnrs_data_raw()

lnrs_data_raw %>% 
  fill(Theme, .direction = "down") %>% 
  clean_names() %>% 
  pivot_longer(cols = starts_with("data"),
               names_to = "status",
               values_to = "description", 
               names_prefix = "data_") %>% 
  arrange(theme, status) %>% 
  filter(!is.na(description)) %>% 
  write_csv('data/lnrs_data_long.csv')

# get_lnrs_data_complete <- function() {
#   tibble(
# 
#     theme = c("Accessible Green Space","Accessible Green Space",
#                                              "Biodiversity Net Gain","GI Provision",
#                                              "GI Provision","Green Jobs",
#                                              "Habitat extent","Habitat extent",
#                                              "Land managed for nature",
#                                              "Land managed for nature",
#                                              "Quality of protected sites network",
#                                              "Quality of protected sites network",
#                                              "Sustainable food growing",
#                                              "Sustainable food growing","Sustainable food growing",
#                                              "Sustainable food growing",
#                                              "Trees and Woodland",
#                                              "Trees and Woodland","Trees and Woodland",
#                                              "Water Quality and Quantity",
#                                              "Water Quality and Quantity",
#                                              "Water Quality and Quantity",
#                                              "Water Quality and Quantity",
#                                              "Wildlife abundance/distribution",
#                                              "Wildlife abundance/distribution",
#                                              "Wildlife abundance/distribution"),
#                              description = c("Access to Natural Green Space (ANGST)",
#                                              "Area of OS open green space","Net Gain Metric",
#                                              "Investment in GI Projects",
#                                              "Number of GI Projects funded",
#                                              "Number of jobs","Priority habitat mapping",
#                                              "'Living England' Habitat probability mapping",
#                                              "Land in CS/ELM Scheme",
#                                              "Area of designated sites and nature reserves",
#                                              "SSSI Status for region",
#                                              "Woodland ecological condition","Land in CS/ELM Scheme",
#                                              "Number of allotments","Soil Health",
#                                              "People growing own food",
#                                              "Canopy Cover (Forestry Research Tool)",
#                                              "Trees for Climate Monitoring",
#                                              "Other remote sensing",
#                                              "Sub-catchment water quality","Fish barriers",
#                                              "eDNA Sampling?",
#                                              "Real-time monitoring",
#                                              "Avon Ornithological Club bird surveys","BRERC Distribution data",
#                                              "Wildlife index"),
#                                available = c(TRUE,TRUE,FALSE,
#                                              TRUE,TRUE,FALSE,TRUE,TRUE,TRUE,
#                                              TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,
#                                              FALSE,TRUE,TRUE,FALSE,TRUE,
#                                              TRUE,FALSE,FALSE,TRUE,TRUE,FALSE),
#                               accessible = c(TRUE,TRUE,FALSE,
#                                              TRUE,TRUE,FALSE,FALSE,TRUE,FALSE,
#                                              TRUE,TRUE,FALSE,TRUE,TRUE,
#                                              FALSE,FALSE,FALSE,TRUE,FALSE,TRUE,
#                                              TRUE,FALSE,FALSE,FALSE,FALSE,
#                                              FALSE),
#                                   status = c("existing","existing",
#                                              "future","existing","existing",
#                                              "future","existing","existing",
#                                              "existing","existing","existing",
#                                              "existing","existing","existing",
#                                              "future","future","existing",
#                                              "existing","future","existing",
#                                              "existing","future","future",
#                                              "existing","existing","future"),
#                                lnrs_2023 = c(TRUE,TRUE,FALSE,
#                                              FALSE,FALSE,FALSE,TRUE,TRUE,FALSE,
#                                              TRUE,TRUE,TRUE,FALSE,FALSE,
#                                              FALSE,FALSE,TRUE,FALSE,TRUE,
#                                              TRUE,TRUE,FALSE,FALSE,TRUE,TRUE,
#                                              TRUE),
#                                lnrs_2024 = c(TRUE,TRUE,TRUE,TRUE,
#                                              TRUE,FALSE,TRUE,TRUE,TRUE,
#                                              TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,
#                                              TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,
#                                              TRUE,TRUE,TRUE,TRUE,TRUE),
#                                 platform = c(TRUE,TRUE,TRUE,TRUE,
#                                              FALSE,FALSE,TRUE,TRUE,TRUE,
#                                              TRUE,FALSE,TRUE,TRUE,TRUE,FALSE,
#                                              FALSE,TRUE,TRUE,TRUE,TRUE,
#                                              TRUE,TRUE,TRUE,TRUE,TRUE,TRUE)
#                       )
# }
# 
# lnrs_data_complete <- get_lnrs_data_complete()

lnrs_data_complete %>% 
  write_csv('data/lnrs_data_complete.csv')

lnrs_data_complete <- read_csv('data/lnrs_data_complete.csv')

make_col_bool <- function(bool){

  return(if_else(bool, 'lightgreen', '#F28482'))
}

make_col_status <- function(status){
  
  return(if_else(status == 'Existing', 'lightgreen', 'orange'))
}


light_grey <- function(x) 'lightgrey'


lnrs_data_table_gt <- lnrs_data_complete %>%
  relocate(theme, description, status, everything()) %>%
  mutate(status = str_to_title(status)) %>% 
  group_by(theme) %>% 
  gt() %>% 
  data_color(columns = c(available, accessible, lnrs_2023, lnrs_2024, platform),
             fn = make_col_bool) %>% 
  data_color(columns = status, fn = make_col_status) %>% 
  data_color(columns = 1:2, fn = light_grey) %>% 
  cols_label_with(columns = !starts_with('lnrs'), 
                  fn = str_to_title) %>% 
  cols_label_with(columns = starts_with('lnrs'), 
                  fn = compose(str_to_upper,
                               ~str_replace(., "_", " "))) %>% 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>% 
  tab_style(
    style = cell_text(weight = 'bold'),
    locations = cells_row_groups()
  )


gtsave(lnrs_data_table_gt, filename = 'plots/lnrs_data_table.png')  


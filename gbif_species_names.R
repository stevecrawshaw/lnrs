pacman::p_load(tidyverse,
               httr2)


httr2::sta

species_vec <- read_rds("data/species_vec.rds")

url <- "https://api.gbif.org/v1/species/match"

match_species <- function(scientific_name){
  request(url) %>% 
  req_headers("Accept" = "application/json") %>% 
  req_url_query(verbose = FALSE,
                name = scientific_name) %>% 
  req_perform() %>% 
  resp_body_json() %>%
  as_tibble() 

}

species_tbl <- species_vec %>% 
  map(~match_species(.x)) %>% 
  bind_rows()

pacman::p_load(tidyverse, glue, janitor, rvest)

csht_url <- "https://www.gov.uk/government/publications/countryside-stewardship-higher-tier-get-ready-to-apply/countryside-stewardship-higher-tier-actions"

csht_cap_url <- "https://www.gov.uk/government/publications/countryside-stewardship-higher-tier-get-ready-to-apply/countryside-stewardship-higher-tier-new-capital-items"


grant_scheme_from_url <- function(url){

  url |> 
  str_match("y/count.+$") |> 
  str_remove("y/") |> 
  str_split("-", simplify = TRUE) |> 
  as.vector() |> 
  map_chr(~substr(.x, 1, 1) |> 
            str_to_upper()) |> 
            str_c(collapse = "")
}




make_csht_tbl <- function(url){
  # return a table of grants and details from a url of the countryside
  # stewardship page. There are 2 here: Higher Tier and Capital Items
  gs = grant_scheme_from_url(url)
  
  url |>
  read_html(url) |> 
  html_nodes("h3") |> 
  html_attr("id") |> 
  discard(is.na) |> 
  tibble() |> 
  set_names("action_id") |> 
  mutate(grant_scheme = gs)
 
}

csht_tbl <- list(csht_url, csht_cap_url) |> 
  map(make_csht_tbl) |> 
  bind_rows() 


csht_tbl |> 
   mutate(grant_id = str_split_i(action_id, "-", i = 1) |> str_to_upper(),
         url = glue("{csht_url}#{action_id}"),
         desc = map(action_id, ~str_split(.x, "-", simplify = TRUE) |>
                      discard_at(1) |> 
                      str_c(collapse = " ") |> 
                      str_to_sentence()),
         grant_summary = glue("{grant_id}: {desc}"),
         desc = NULL,
         action_id = NULL
           
  ) |> glimpse()


d = c("cat", "sat", "on", "mat")

d[2:length(d)]
d |> keep_at(2:length(d)) 


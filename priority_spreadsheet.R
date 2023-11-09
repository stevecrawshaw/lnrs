pacman::p_load(tidyverse,
               glue,
               janitor,
               fs,
               sf,
               readxl)


sheets_vec <-
  readxl::excel_sheets('data/local_priorities_master.xlsx') %>%
  setdiff("Sheet6")

read_wrangle_sheet <- function(sheet_name) {
  range <- "B4:K20"
  col_names <- letters[2:11]
  
  read_xlsx(
    'data/local_priorities_master.xlsx',
    sheet = sheet_name,
    range = range,
    col_names = col_names
  ) %>%
    filter(!is.na(b), b != "Priority") %>%
    transmute(Name = sheet_name,
              Priority = b,
              Area = k)
  
}

cons_tbl <- sheets_vec %>%
  map( ~ read_wrangle_sheet(.x)) %>%
  bind_rows()

cons_tbl %>% 
  # mutate(Priority = str_remove_all(Priority, "\\'")) %>% 
  write_tsv("data/single_sheet_priority.tsv")

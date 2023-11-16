pacman::p_load(tidyverse,
               glue,
               janitor,
               fs,
               sf,
               readxl)
# this is for consolidating all the areas into a single sheet
spreadsheet_path <- 'data/local_priorities_master.xlsx'

sheets_vec <-
  readxl::excel_sheets(spreadsheet_path) %>%
  setdiff("Sheet6")

read_wrangle_sheet <- function(sheet_name, spreadsheet_path, range) {
  # range <- "B4:K20"
  col_names <- letters[2:11]
  
  read_xlsx(
    spreadsheet_path,
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
  map( ~ read_wrangle_sheet(.x, spreadsheet_path, range = "B4:K20")) %>%
  bind_rows()

cons_tbl %>% 
  # mutate(Priority = str_remove_all(Priority, "\\'")) %>% 
  write_tsv("data/single_sheet_priority.tsv")

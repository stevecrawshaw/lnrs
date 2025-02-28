pacman::p_load(tidyverse,
               sf,
               glue,
               janitor,
               readxl)

workbook_path <- "data/Priorities and Measures Master for portal_2025 Stewardship update.xlsx"

make_reference_id_tbl <- function(raw_tbl, name){
  id_col = glue("{name}_id")
  raw_tbl |> 
    t() |>
    as_tibble() |>
    set_names(name) |>
    rownames_to_column(id_col) |>
    mutate(across(ends_with("id"), as.integer))
}

sheets <- readxl::excel_sheets(workbook_path)

benefits_raw_tbl <- read_xlsx(workbook_path,
                          sheet = "references",
                          range = "A1:N2") |> 
  make_reference_id_tbl("benefit")

valid_benefits_row_index <- min(which(benefits_tbl$benefit == "0")) -1

benefits_tbl <- benefits_raw_tbl |> 
  head(valid_benefits_row_index)

benefit_names <- benefits_tbl$benefit

species_tbl <- read_xlsx(workbook_path,
                          sheet = "references",
                          range = "A5:AY6") |> 
  make_reference_id_tbl("species")

# Get benefits lookup tbl ----

measures_benefits_ids_tbl <- read_xlsx(workbook_path,
                                   sheet = "Measures & Co-benefits",
                                   ) |> 
  select(`Measure ID`, any_of(benefit_names)) |>
  mutate(across(
    .cols = any_of(benefit_names),
    ~ if_else(
      .x %in% c("x", "X"),
      benefits_tbl$benefit_id[benefits_tbl$benefit == cur_column()],
      NA_integer_))) |> 
  glimpse()

measures_benefits_lookup_tbl <- 
  measures_benefits_ids_tbl |> 
  pivot_longer(cols = -`Measure ID`,
               names_to = "benefit",
               values_to = "benefit_id") |>
  select(-benefit) |> 
  filter(!is.na(benefit_id)) |> 
  clean_names() 

# Get species lookup tbl ----
# 
species_measures_ids_tbl <- read_xlsx(workbook_path,
                                   sheet = "Measures & Species",
                                   ) |> 
  select(`Measure ID`, any_of(species_tbl$species)) |>
  mutate(across(
    .cols = any_of(species_tbl$species),
    ~ if_else(
      .x %in% c("x", "X"),
      species_tbl$species_id[species_tbl$species == cur_column()],
      NA_integer_))) |> 
  glimpse()

species_measures_lookup_tbl <- 
  species_measures_ids_tbl |> 
  pivot_longer(cols = -`Measure ID`,
               names_to = "species",
               values_to = "species_id") |>
  select(-species) |> 
  filter(!is.na(species_id)) |> 
  clean_names()


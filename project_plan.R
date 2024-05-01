pacman::p_load(tidyverse,
               glue,
               janitor,
               gt,
               ganttrify,
               readxl)
# Variables ----
weca_core_colours <- c(
  "Dark Blue" = "#354753",
  "Blue" = "#6bd4f2",
  "Pink" = "#ed749d",
  "Yellow" = "#ffd900",
  "Green" = "#79decc") %>% 
  unname()

date_label <- Sys.Date() %>% 
  as.character()  %>% 
  str_replace_all("\\s|\\.|\\:|\\-", "")

# from monitoring framework state of nature:: project plan LOCAL

lnrs_tbl <- read_xlsx('data/project_plan.xlsx',
          sheet = 'project_plan_2')


# Functions ----

read_spot_tbl <- function(filename = "data/project_plan.xlsx", 
                          sheetname) {
  read_xlsx(filename, sheet = sheetname) %>% 
    mutate(spot_date = as.Date(spot_date),
           spot_type = toupper(spot_type))
}

# make clean table for ganttrification
make_clean_tbl <- function(tbl) {
  tbl %>% 
    filter(!is.na(activity),
           !is.na(start_date)) %>% 
    fill(wp, .direction = "down") %>% 
    mutate(across(.cols = ends_with("date"), as.Date))
}

# Make GT table for detailed plan

make_activity_table_gt <- function(tbl) {
  tbl %>% 
    select(wp, activity, detail = Detail, start_date, end_date) %>%
    mutate(across(.cols = ends_with("date"), .fns = ~format(.x, "%b %y")),
           detail = if_else(is.na(detail), "", detail)) %>% 
    group_by(wp) %>% 
    gt() %>% 
    cols_label_with(columns = ends_with("date"), 
                    fn = compose(~str_replace(., "_", "\n"), str_to_title)) %>% 
    cols_label_with(columns = everything(), 
                    fn = str_to_title) %>% 
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
    ) %>% 
    tab_style(
      style = cell_text(weight = 'bold', size = 'large'),
      locations = cells_row_groups()
    )  
}

my_ganntrify <- function(clean_tbl,
                      spot_tbl,
                      core_colours = weca_core_colours,
                      title,
                      caption) {
  clean_tbl %>% 
    ganttrify(
      label_wrap = 30,
      colour_palette = core_colours,
      spots = spot_tbl,
      month_number_label = FALSE,
      alpha_activity = 0.6,
      by_date = TRUE,
      size_text_relative = 1.2, 
      mark_quarters = TRUE) +
    labs(title = title,
         caption = caption)

}

save_gantt <- function(gantt_object, filename, date_label, width = 15, height = 14){
  path = glue("plots/gantt_{filename}_{date_label}.png")
  ggsave(path,
         device = "png",
         plot = gantt_object,
         bg = "white", 
         width = width,
         height = height)
  return(path)
}

# create the output ----
clean_tbl <- make_clean_tbl(lnrs_tbl) %>% 
  filter(primary == 4)

spot_tbl <- read_spot_tbl(sheetname = "m_and_e_spot")


m_and_e_gantt <- my_ganntrify(clean_tbl,
                              spot_tbl,
                              title = "Open Data Portal: M&E Activities",
                              caption = "U = Update\nR = Reporting\nS = Survey")

m_and_e_gantt

save_gantt(m_and_e_gantt,
           "m_and_e",
           width = 10,
           height = 11,
           date_label)


# Output project plan
activity_table_gt %>% 
  gtsave(glue("plots/activity_table_portal_{date_label}.png"))  


gantt_chart
# Save




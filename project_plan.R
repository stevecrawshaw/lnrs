pacman::p_load(tidyverse,
               glue,
               janitor,
               gt,
               ganttrify,
               readxl)

# from monitoring framework state of nature:: project plan LOCAL

lnrs_tbl <- read_xlsx('data/Monitoring Framework_State of data_GI WG.xlsx',
          sheet = 'project_plan')

spot_lnrs_tbl <- read_xlsx('data/Monitoring Framework_State of data_GI WG.xlsx',
                           sheet = "project_spot_data") %>% 
  mutate(spot_date = as.Date(spot_date),
         spot_type = toupper(spot_type))

# make clean table for ganttrification
lnrs_clean_tbl <- 
  lnrs_tbl %>% 
  filter(!is.na(activity),
         !is.na(start_date)) %>% 
  fill(wp, .direction = "down") %>% 
  mutate(across(.cols = ends_with("date"), as.Date)) 

# Make GT table for detailed plan

activity_table_gt <- lnrs_clean_tbl %>% 
  select(wp, activity, detail = Detail, start_date, end_date) %>%
  mutate(across(.cols = ends_with("date"), .fns = ~format(.x, "%b %y"))) %>% 
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
  ) %>% 
  tab_style(
    style = list(
      cell_fill(color = "#daa3ae"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = activity,
      rows = str_detect(activity, "DECISION")
    )
  )
  
# Output project plan
activity_table_gt %>% 
  gtsave('plots/activity_table.png')  


weca_core_colours <- c(
  "Dark Blue" = "#354753",
  "Blue" = "#6bd4f2",
  "Pink" = "#ed749d",
  "Yellow" = "#ffd900",
  "Green" = "#79decc") %>% 
  unname()

# Ganttrify

gantt_chart <- ganttrify(project = lnrs_clean_tbl,
         label_wrap = 30,
         colour_palette = weca_core_colours,
         spots = spot_lnrs_tbl,
         month_number_label = FALSE,
         alpha_activity = 0.6,
         by_date = TRUE,
         size_text_relative = 1.2, 
         mark_quarters = TRUE) +
  labs(title = "Draft LNRS and Nature Data Project Plan",
       caption = "M = Milestone: D = Decision")

# Save
ggsave('plots/gantt.png',
       device = 'png',
       plot = gantt_chart,
       bg = "white", 
       width = 10,
       height = 12)



pacman::p_load(tidyverse)


# Read the file
lines <- readLines("data/Statements09012911179157.txt", encoding = "utf8")

st_tbl <- lines %>% 
  map_chr(~str_remove_all(.x, "\\\xa0|\\t")) %>% 
  as_tibble() %>% 
  filter(!value == "") %>% 
  separate(col = value, into = c("name", "value"), sep = ":", remove = TRUE) %>% 
  pivot_wider(names_from = name, values_from = value, values_fn = list) %>% 
  unnest(cols = everything())

st_tbl %>% 
  filter(str_detect(Description, "James"))
  

# THIS IS NOW INCORPORATED INTO THE PROCESS_PRIORITY_SHEETS.R SCRIPT

# # construct tables of grants and links to be joined to recommendations
# # Countryside Stewardship Grant Links Table ----
# make_url_vec <- function(base_url = "https://www.gov.uk/countryside-stewardship-grants", num_pages = 6){
# 
# pages_url <- paste0(rep(base_url, num_pages -1), "?page=", 2:num_pages)
# 
# return(c(base_url, pages_url))
# }
# 
# get_links <- function(url){
# # Read the HTML content of the page
#   page <- read_html(url)
#   # Find all link nodes
#   link_nodes <- html_nodes(page, "a")
#   # output a tibble
#   tibble(
#     text = html_text(link_nodes),
#     url = html_attr(link_nodes, "href"))
# }
# 
# make_links_raw_tbl <- function(make_url_vec, get_links){
#   make_url_vec() %>% 
#     map(get_links) %>% 
#     bind_rows()
# }
# 
# make_links_tbl <- function(links_raw_tbl, domain = "https://www.gov.uk"){
# # clean and wrangle the links to get just the guidance links
#   links_raw_tbl %>% 
#   mutate(grant_name = str_trim(text, side = "both")) %>% 
#     # need to filter for 2 cap letters, numbers colon **and** 2 cap letters, numbers, space
#   filter(str_detect(grant_name, pattern = "^[A-Z]{2}\\d{0,2}:|^[A-Z]{2}\\d{0,2}\\s")) %>% 
#   mutate(url = glue("{domain}{url}"), 
#          text = NULL,
#          grant_id = str_extract(grant_name, "^[^:]+") %>% 
#            str_extract("^\\w+"),
#          category = "cs",
#          grant_scheme = "Countryside Stewardship",
#          annual_payment = "") # flipping missing colon BC3 BC4
# }
# 
# links_raw_tbl <- make_links_raw_tbl(make_url_vec, get_links)
# cs_links_tbl <- make_links_tbl(links_raw_tbl = links_raw_tbl,
#                             domain = "https://www.gov.uk")
# 
# # Sustainable Farming Initiative Grants Links Table ----
# 
# # from Table 1 here https://assets.publishing.service.gov.uk/media/6516c0986a423b0014f4c62e/SFI23_handbook.pdf
# # via chatGPT to parse into table 
# # https://chat.openai.com/share/65eac56b-a023-4129-9ea2-add1d6d24f88
# 
# sfi_raw_tbl <- read_delim('data/chat_tbl.txt', delim = "|")
# 
# clean_sfi_tbl <- function(sfi_raw_tbl, url = "https://assets.publishing.service.gov.uk/media/6516c0986a423b0014f4c62e/SFI23_handbook.pdf"){
# 
#   sfi_raw_tbl %>% 
#   select(2:5) %>% 
#   clean_names() %>% 
#   filter(!str_detect(code, "--")) %>% 
#   mutate(across(.cols = everything(), ~str_trim(.x, side = "both"))) %>%
#   mutate(url = url,
#          grant_name = glue("{code}: {sfi_action}"),
#          grant_scheme = "Sustainable Farming Initiative",
#          grant_id = code,
#          ) %>% 
#   select(url, grant_name, grant_id, category, grant_scheme, annual_payment) 
# 
# }
# 
# sfi_tbl <- clean_sfi_tbl(sfi_raw_tbl = sfi_raw_tbl)
# 
# # Consolidate into One Table ----
# 
# all_grants_tbl <- bind_rows(cs_links_tbl, sfi_tbl)


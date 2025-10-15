# CODE REMOVED FROM THE MAIN PROCESSING SCRIPT WHEN SFI AND CS WERE CHANGED APRIL 2025
# check url and linkchecker can go if SFI and CS removed

check_url <- function(url) {
tryCatch({
  # Create and perform the request
  response <- request(url) |>
    req_perform()
  
  # Check the status code
  resp_status(response)

}, error = function(e) {
  0
})
}

linkchecker <- function(x){
# check if a link is valid
# return a character string 200 if ok, otherwise the http_*
# R error code as httr2 doesn't return the native 
# API error code
tryCatch(
  expr = {
    request(x) |> 
      req_perform() |> 
      resp_status() |> 
      as.character()
    #message("Successfully checked the link.")
  },
  error = function(e){
    #message('Caught an error!')
    class(e)[1] 
  },
  warning = function(w){
    #message('Caught an warning!')
    class(w)[1]
  },
  finally = {
    #message('All done, quitting.')
  }
)    
}

# v this can go too
check_valid_grant_string <- function(x, letters = 2){

if (letters == 2){
  pattern = "^[A-Z]{2}\\d+$"
} else if (letters == 3) {
  pattern = "^[A-Z]{3}\\d+$"
}
str_detect(x, pattern)
}
# v this can go
rename_action_col <- function(tbl){
  # utility function to rename the disparate action columns
  tbl |> 
    rename_with(\(x) "sfi_action",
                .cols = starts_with("SFI")) 
}
# v and this
make_grants_tbl <- function(cs_tbl,
                          sfi_tbl,
                          ofc_clean_tbl,
                          cs_grant_codes_tbl){

bind_rows(cs_tbl, sfi_tbl, ofc_clean_tbl) |> 
  mutate(code_prefix = if_else(str_starts(grant_id, "[A-Z]{2}"),
                               str_extract_all(grant_id, "[A-Z]") |> 
                                 map_chr(~paste0(.x, collapse = "")), "")) |> 
  left_join(cs_grant_codes_tbl,
            by = join_by(code_prefix == code_prefix)) |> 
  mutate(code_prefix = NULL,
         link_status = NULL) |> 
  add_id() |> 
  relocate(grant_id, id, url, grant_name, grant_scheme) |> 
  mutate(grant_summary = glue(
    "Grant name: {grant_name}\n
   Grant scheme: {grant_scheme}\n
   Link: <a href={url} target=_blank>{url}</a>\n
   url: {url}"))

}
# V this can go?
parse_cs_grant_codes <- function(sheets_list){

sheets_list |> 
  pluck("farming_codes") |> 
  filter(scheme == "Countryside Stewardship") |> 
  mutate(category = str_to_sentence(meaning),
         meaning = NULL,
         scheme = NULL) |> 
  rename(code_prefix = code)
}

# construct tables of grants and links to be joined to measures
# Countryside Stewardship Grant Links Table
# V I think this can go
make_url_vec <- function(base_url = "https://www.gov.uk/countryside-stewardship-grants", num_pages = 6){

pages_url <- paste0(rep(base_url, num_pages -1), "?page=", 2:num_pages)

return(c(base_url, pages_url))
}

get_links <- function(url){
  # Read the HTML content of the page
  page <- read_html(url)
  # Find all link nodes
  link_nodes <- html_nodes(page, "a")
  # output a tibble
  tibble(
    text = html_text(link_nodes),
    url = html_attr(link_nodes, "href"))
}

make_links_raw_tbl <- function(make_url_vec, get_links){
  make_url_vec() |> 
    map(get_links) |> 
    bind_rows()
}

make_cs_tbl <- function(links_raw_tbl, domain = "https://www.gov.uk"){
  # clean and wrangle the links to get just the guidance links
  links_raw_tbl |> 
    mutate(grant_name = str_trim(text, side = "both")) |> 
    # need to filter for 2 cap letters, numbers colon **and** 2 cap letters, numbers, space
    filter(str_detect(grant_name, pattern = "^[A-Z]{2}\\d{0,2}:|^[A-Z]{2}\\d{0,2}\\s")) |> 
    mutate(url = glue("{domain}{url}"), 
           text = NULL,
           grant_id = str_extract(grant_name, "^[^:]+") |> 
             str_extract("^\\w+"),
           grant_scheme = "Countryside Stewardship")
# flipping missing colon BC3 BC4
}


sfi_2024_url <- "https://www.gov.uk/government/publications/sustainable-farming-incentive-scheme-expanded-offer-for-2024/sfi-scheme-information-expanded-offer-for-2024#annex-b-summary-of-the-initial-expanded-sfi-offer-from-summer-2024"

make_sfi_web_tbl <- function(sfi_url = sfi_2024_url){

rvest::read_html(sfi_url) |> 
html_table() |> 
keep(~ncol(.x) == 4) |> 
map(rename_action_col) |> 
bind_rows() |> 
clean_names() |> 
rename(grant_id = code)

}

sfi_web_tbl <- make_sfi_web_tbl(sfi_2024_url)
# funding source links

find_funding_base_url <- "https://www.gov.uk/find-funding-for-land-or-farms"

funding_base_urls <- paste0(find_funding_base_url, c("?page=1", "?page=2", "?page=3"))

get_sfi_vector <- function(funding_base_url){
#extract the list of grant links given a page of grants
funding_base_url |> 
  read_html() |> 
  html_elements(css = ".gem-c-document-list__item  ") |> 
  html_text2()
}

get_sfi_stub <- function(item){
# get the first item which contains the grant code, summary
# and can be used to create the URL for the grant
item |> 
  str_split("\n") |> 
  pluck(1, 1)
}

make_url_from_stub <- function(stub, base_url){
# do various cleaning operations on the stub
# and make a valid url for the grant
stub_reformatted <- stub |> 
  str_replace_all(pattern = ":\\s|\\s",
                  replacement = "-") |> 
  str_remove_all("\\(|\\)|,")|>
  str_replace("%", "-percent") |>
  str_remove("–") |> # this dash is not the same as the others
  str_replace("--", "-") |> 
  tolower()

paste0(base_url, "/", stub_reformatted)
}

grant_name <- funding_base_urls |> 
map(get_sfi_vector) |> 
map(~map_chr(.x, get_sfi_stub)) |> 
unlist() 

grant_urls <- grant_name |> 
map_chr(~make_url_from_stub(.x, find_funding_base_url))

# lcheck <- map_chr(t, linkchecker) |> 
#   map_lgl(~.x == "200")
# 
# t[!lcheck]

sfi_funding_tbl <- tibble(grant_name, url = grant_urls) |> 
separate(grant_name,
         into = c("grant_id", "summary"),
         sep = ": ",
         remove = FALSE) |> 
mutate(grant_scheme = "SFI",
       summary_wrapped = str_wrap(summary,
                                  whitespace_only = TRUE, 
                                  width = 50),
       link_status = map(url, linkchecker) |>
         as.integer()) |> 
glimpse()

sfi_tbl <- sfi_web_tbl |> 
left_join(sfi_funding_tbl,
          by = join_by(grant_id)) |> 
select(-c(sfi_action, annual_payment, action_s_duration)) |> 
glimpse()

# countryside stewardship and make final grants tbl

links_raw_tbl <- make_links_raw_tbl(make_url_vec, get_links)

cs_tbl <- make_cs_tbl(links_raw_tbl = links_raw_tbl,
                    domain = "https://www.gov.uk") |> 
mutate(link_status = map_int(url, check_url))

# TRUE if all links good
if(all(cs_tbl$link_status == 200)){
TRUE
} else {
cs_tbl |> filter(link_status != 200)
}

cs_grant_codes_tbl <- parse_cs_grant_codes(sheets_list)


# deprecated
# grants_tbl <- make_grants_tbl(
#   cs_tbl, #drop
#   sfi_tbl, #drop 
#   ofc_clean_tbl,
#   cs_grant_codes_tbl
#   ) |> 
# glimpse()
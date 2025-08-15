pacman::p_load(tidyverse, glue, janitor, magick, glue)
# this script checks the urls provided by vaishnavi for the images hosted on dash
# Get the species list
#
species_image_dash_raw_tbl <- read_csv("data/images_out_dash_tbl.csv")

url_tbl <- species_image_dash_raw_tbl |>
  select(final_file_name, URLs)

downit <- function(final_file_name, URLs) {
  download.file(
    URLs,
    destfile = glue("data/image_check/{final_file_name}"),
    method = "curl"
  )
}

url_tbl |>
  pwalk(downit)


download.file(urls[1], destfile = "data/image_check/adder.jpg", method = "curl")

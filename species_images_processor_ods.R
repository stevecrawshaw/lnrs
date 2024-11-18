pacman::p_load(tidyverse,
               glue,
               janitor,
               magick,
               collapse,
               fs)

# extract the single image from each sub folder
# and save it to the parent folder
# rename the image to the name of the sub folder appended to the original file  name
# so that the file name can be joined onto the species table and loaded onto the portal
# also create a smaller image file (thumbnail) for indicative display on the LNRS app
# and extract the licence into a separate field
# 

parent_folder <- fs::path("data", "portal_upload", "species_images_folders")

sub_folders <- fs::dir_ls(parent_folder)

resize_write <- function(write_path_full, write_path_thumbnail){
  image_read(write_path_full) |> 
    image_scale("200x200") |> 
    image_write(write_path_thumbnail)
}

# A function to extract the single image from each sub folder
# 

images_raw_tbl <- sub_folders |> 
map(~fs::dir_ls(.x, glob = "*.jpg|*.png") |> 
      path_split()) |>
collapse::unlist2d()

image_upload_path = fs::path("data",
                               "portal_upload",
                               "images_for_upload")

images_out_tbl <- images_raw_tbl |> 
  select(1, 6, 7) |> 
  set_names("full_path", "folder", "file_name") |>
  mutate(folder_components_list = map(folder, ~str_split(., "__")),
         common_name = map_chr(folder_components_list, ~.x[[1]][1]),
         usage_key = map_chr(folder_components_list, ~.x[[1]][2]),
         folder_components_list = NULL) |> 
  mutate(amended_file_name = map_chr(
    file_name,
    ~str_replace_all(.x,
                     c("_B" = "__B",
                       "(?<!_)_by" = "__by__"))),
    final_file_name = map_chr(
      amended_file_name,
      ~str_replace_all(.x,
                       c("^(.*?(BY|BC).*?)(_)" = "\\1__",
                         "'" = "_")))) |> 
  mutate(license = map_chr(final_file_name,
                           ~str_split_i(.x,
                                        pattern = "__",
                                        2)),
         attribution = map_chr(final_file_name,
                              ~str_split_i(.x,
                                           pattern = "__",
                                           3) |> str_remove("\\.jpg")),
         full_file_path = glue("{full_path}/{file_name}"),
         write_path_full = glue("{image_upload_path}/{usage_key}__{final_file_name}"),
         write_path_thumbnail = glue("{image_upload_path}/{usage_key}__thumbnail__{final_file_name}")
)


images_out_tbl |> 
  pull(full_file_path, write_path_full) |>
  iwalk(file_copy)

images_out_tbl |>
  pull(write_path_full, write_path_thumbnail) |>
  iwalk(resize_write)

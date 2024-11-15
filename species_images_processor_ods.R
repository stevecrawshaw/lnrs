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


# A function to extract the single image from each sub folder
# 

images_raw_tbl <- sub_folders |> 
map(~fs::dir_ls(.x, glob = "*.jpg|*.png") |> 
      path_split()) |>
collapse::unlist2d()

images_out_tbl <- images_raw_tbl |> 
  select(1, 6, 7) |> 
  set_names("full_path", "folder", "file_name") |>
  mutate(folder_components_list = map(folder, ~str_split(., "__")),
         common_name = map(folder_components_list, ~.x[[1]][1]),
         usage_key = map(folder_components_list, ~.x[[1]][2])) |> 
  mutate(amended_file_name = map_chr(file_name,
                                     ~str_replace(.x,
                                                  pattern = "_B",
                                                  replacement = "__B")),
         final_file_name = map_chr(amended_file_name,
                                   ~str_replace(.x,
                                                pattern = "^(.*?(BY|BC).*?)(_)",
                                                replacement = "\\1__"))) |> 
  mutate(license = map_chr(final_file_name,
                           ~str_split_i(.x,
                                        pattern = "__",
                                        2)),
         attribution = map_chr(final_file_name,
                              ~str_split_i(.x,
                                           pattern = "__",
                                           3) |> str_remove("\\.jpg")),
         full_file_path = glue("{full_path}/{file_name}")) 

image_upload_path = fs::path("data",
                               "portal_upload",
                               "images_for_upload")

images_out_tbl |> 
  pull(full_file_path) |>
  walk(~fs::file_copy(.x, image_upload_path))

# Get files with apostrophe first
files <- list.files(image_upload_path, pattern = "'")

# Then rename them
file.rename(
  from = file.path(image_upload_path, files),
  to = file.path(image_upload_path, str_replace(files, "'", "_"))
)

# Now create thumbnails
# 

full_size_image_path <- fs::path("data", "portal_upload", "images_for_upload")

image_files = fs::dir_ls(full_size_image_path, glob = "*.jpg|*.png")


thumbnail_path <- fs::path("data", "portal_upload", "thumbnails")


image_files |> 
  map(~image_read(.x) |> 
        image_scale("200x200") |> 
        image_write(fs::path(thumbnail_path, fs::path_file(.x))))


thumnail_image_files = fs::dir_ls(thumbnail_path, glob = "*.jpg")

file.rename(
  from = thumnail_image_files,
  to = str_replace(thumnail_image_files, "\\.jpg", "_thumbnail.jpg"))


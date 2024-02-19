pacman::p_load(tidyverse,
               sf,
               glue,
               janitor,
               fs,
               rlist)

path <- "data/shapefiles"
# areas_description_tbl <- read_delim("data/portal_upload/areas_tbl.csv",
#                                     delim = ";")

name_layers <- function(shp_paths){

shp_paths %>%
  path_file() %>% 
  path_ext_remove() %>% 
  make_clean_names() %>% 
  str_c("_sf")

}
trans_write <- function(sf_obj, subfolder = "portal_write"){
  # doesn't work with pipes!
  filename_stem <- deparse(substitute(sf_obj)) %>%
    str_replace_all("_", "-")
  
  filenames <- c(glue("data/{subfolder}/{filename_stem}.geojson"), glue("data/{subfolder}/{filename_stem}.gpkg"))
  filenames %>% walk(~st_transform(sf_obj, 
                                  crs = 4326) %>% 
                       st_write(dsn = .x, append = FALSE))
  
}
# get the raw data for processing
unzip_from_onedrive <- function(path = "data/shapefiles",
                                delete_zipfile = TRUE){
  
  zipfile <- fs::dir_ls(path, glob = "*.zip")
  
  filepaths <- unzip(zipfile,
                     junkpaths = TRUE,
                     exdir = path,
                     overwrite = TRUE)
  if (delete_zipfile){
  fs::file_delete(zipfile)
  }
  
return(filepaths)
}


# RAW SHAPEFILES ---
# Get the unprocessed shapefiles from downloaded onedrive zipped file

filepaths <- unzip_from_onedrive(path = path)
# filepaths <- dir_ls(path, glob = "*.shp") # if not unzipping

shp_paths <- filepaths %>% 
  str_subset("shp$")

layer_name <- name_layers(shp_paths)

geo_list <- map(shp_paths, ~st_read(.x, promote_to_multi = TRUE)) %>% 
  set_names(layer_name)

list2env(geo_list, .GlobalEnv)


# GEOMETRICALLY SOUND FILES FOR POLISHING ---

scratch_path <- fs::path("data", "scratch")

layers_list <- fs::dir_ls(scratch_path, glob = "*.geojson") %>% 
  map(~st_read(.))

layer_names <- name_layers(layers_list %>% names())

raw_files_list <- layers_list %>% set_names(layer_names)

list2env(raw_files_list, .GlobalEnv)

lnrs_areas_for_description <- areas_for_description_single_4326_sf %>% 
  select(id) 
# %>% 
#   inner_join(areas_description_tbl, by = join_by(id == area_id))

lnrs_areas_for_description %>% glimpse()

lnrs_areas_important <- areas_important_bd_single_fixed_4326_sf %>% 
  select(geometry)

lnrs_areas_important

lnrs_areas_pi <- pi_singlepart_4326_sf %>% 
  select(geometry)

lnrs_areas_priority <- priority_areas_4326_sf %>% 
  select(id, Name)

lnrs_areas_priority <- 
  st_read("data/scratch/areas_priority_clean.gpkg") %>% 
  select(id, Name) %>% 
  inner_join(areas_tbl, by = join_by(id == area_id))


trans_write(lnrs_areas_priority)
trans_write(lnrs_areas_pi)
trans_write(lnrs_areas_for_description)
trans_write(lnrs_areas_important)

# The successful strategy is to convert to single part "Multipart to Singlepart" in QGIS, CRS to 4326 and export to geoJSON

# This should be the same as st_cast(..., "POLYGON") but that doesn't seem to work


trans_write(picasted)

# https://r-spatial.org/r/2017/03/19/invalid.html

any(is.na(st_is_valid(pi_valid)))

any(na.omit(st_is_valid(pi_valid)) == FALSE)

identical(pi_valid_buff, pi_valid_mp)


trans_write(pi_valid_buff)



valid_sf_list <- map(geo_list, st_make_valid)

multipoly_sf <- master_areas_for_priority_description_simplified_cleaned_sf %>%
  mutate(geom_type = st_geometry_type(.)) %>% 
  filter(geom_type != "POLYGON")




trans_write(multipoly_sf, "data/fixed/multipoly.geojson")
trans_write(poly_sf, "data/fixed/poly.geojson")
#----
# Multipart to single part in qgis
# fix geometry
#  filter(!st_is_empty(.))

# just 2 errors - self intersections

# but missing avon and wickwar


#----

pacman::p_load(tidyverse,
               glue,
               janitor,
               fs,
               zip)


sourcepath <- 'data/st'
destpath <- 'data/dem'

fs::dir_create(destpath)

files <- fs::dir_ls(sourcepath)

walk(files, ~unzip(.x, exdir = destpath, overwrite = TRUE))

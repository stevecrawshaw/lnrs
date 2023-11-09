pacman::p_load(tidyverse,
               glue,
               janitor,
               sf,
               DBI,
               RPostgres,
               config)

creds <- config::get()


tryCatch({
  # drv <- dbDriver("PostgreSQL")
  print("Connecting to Database…")
  connec <- dbConnect(drv = RPostgres::Postgres(),
                      dbname = creds$database,
                      host = creds$host, 
                      port = creds$port,
                      user = creds$user, 
                      password = creds$pass)
  print("Database Connected!")
},
error=function(cond) {
  print("Unable to connect to Database.")
})

dbListObjects(connec, prefix = Id(schema = 'brerc'))

dbListFields(connec,
             name = Id(schema = "brerc",
             table = "habitat_map"))

dbListTables(connec, "brerc")


habmap_tbl <- tbl(connec, dbplyr::in_schema(schema = 'brerc', table = 'habitat_map')) %>% 
  head(100) %>% 
  collect()

dbDisconnect(connec)

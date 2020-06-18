
#######################################
#       SQL Server meta tables        #
#######################################

library(googlesheets4)
library(googledrive)
library(tidyverse)
library(lubridate)
library(DBI)
library(jsonlite)

drive_auth()
sheets_auth(token = drive_token())

meta_tbl <- drive_find(pattern = "(moab_db)")   # add "_meta"

pi <- sheets_read(meta_tbl[3, ], sheet = 1) %>% 
    mutate_at(c("start_date", "end_date"), ymd) %>% 
    filter(last_name == "Michaud")

proj <- sheets_read(meta_tbl[2, ], sheet = 1) %>% 
    rename(proj_id = project_id) %>% 
    filter(!is.na(proj_code))

pi_proj <- sheets_read(meta_tbl[1, ], sheet = 1) 

dbWriteTable(con, "principal", pi, append = TRUE) 
dbWriteTable(con, "project", proj, append = TRUE)
dbWriteTable(con, "pi_proj", pi_proj, append = TRUE)

pi_p <- tbl(con, "principal") 
pr_p <- tbl(con, "project") 
pipr_p <- tbl(con, "pi_proj") 

ck <- inner_join(pi_p, pipr_p, by = "principal_id") %>% 
    inner_join(pr_p, by = "proj_id") %>% 
    collect()

con2 <- dbConnect(RSQLite::SQLite(), "c:/Users/cmichaud/project/SQLite_db/123d.db")

site_effort <- tbl(con2, "site") %>% 
    collect() %>% 
    mutate(sample_id = site_id,
           site_id = paste("s", site_id, sep = "-"))
site <- site_effort %>% 
    mutate(pi_proj_id = 1) %>% 
    select(site_id, pi_proj_id, river, reach, crew, site_notes)
dbWriteTable(con, "site", site, append = TRUE)

s <- tbl(con, "site") %>% 
    collect()

#----------------------------------

effort <- site_effort %>% 
    mutate(data_type = "EL",
           gear = "EL") %>% 
    nest(sample_json = c(startdatetime, enddatetime, start_rmi, end_rmi, shoreline, el_sec, boat)) %>% 
    select(sample_id, site_id, data_type, gear, sample_json) %>% 
    mutate(sample_json = map_chr(sample_json, toJSON, dataframe = "rows"))    # here "rows" seems correct???

str_sub(effort$sample_json, start = 1, end = 1) <- ""                       # remove opening `[`
str_sub(effort$sample_json, start = -1, end = -1) <- ""                     # remove closing `]`

dbWriteTable(con, "sample", effort, append = TRUE)
e <- tbl(con, "sample") %>% 
    collect() %>% 
    mutate(sample_json = paste0("[", sample_json, "]"),
           sample_json = map(sample_json, fromJSON)) %>% 
    unnest(cols = sample_json) %>% 
    mutate_at(c("startdatetime", "enddatetime"), ymd_hms) 


water <- tbl(con2, "water_qual") %>% 
    collect() %>% 
    select(-c(water_id, key_a)) %>% 
    rename(temp = rvr_temp)
names(water)
dbListFields(con, "water_qual")
dbWriteTable(con, "water_qual", water, append = TRUE)

w <- tbl(con, "water_qual") %>% 
     collect()
 

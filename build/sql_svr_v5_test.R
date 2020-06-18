
#################################################################
#                   SQL Server database build                   #
#################################################################

library(DBI)        # manages database connection
library(tidyverse)      # data manipulations
library(dbplyr)     # extends dplyr functions to database 
library(lubridate)  # date-time manipulations
library(jsonlite)
library(stringr)

con <- DBI::dbConnect(odbc::odbc(), 
                      Driver = "SQL Server", 
                      Server = "localhost\\SQLEXPRESS", 
                      Database = "moab_db", 
                      Trusted_Connection = "True")

#db_list_tables(con)
#dbDisconnect(con)

#--------------------------------------------
# Principal table
#--------------------------------------------

# Principal tibble
principal <- tibble(
    "principal_id" = c(1:5),
    "first_name" = c("Katie", "Brian", "John", "Christopher", "Zach"),
    "last_name" = c("Creighton", "Hines", "Caldwell", "Michaud", "Ahrens"),
    "start_date" = as.Date(c("2007-01-01", "2013-03-03", "2016-10-01", "2013-03-25", "2015-07-19")), 
    "end_date" = as.Date(rep(NA, 5)), 
    "title" = c("Project leader", rep("Biologist", 4))
)

# write data to db
#dbWriteTable(conn = con, name = "principal", value = principal, append = TRUE)

# Collect data from db
pi <- tbl(con, "principal") %>% 
    collect() %>% 
    mutate_at(c("start_date", "end_date"), as.Date)


#--------------------------------------------
# Project table
#--------------------------------------------

dbListFields(con, "project")
# Project table
project <- tibble(
    "project_id" = c(1:6),
    "project_abbr" = c("123a", "123d", "129", "130", "132", "SJHg"),
    "proj_title" = c("Smallmouth bass mgt", "Walleye mgt", "Deso-chub", "Cat-chub", "Westwater-chub", "San Juan mercury"),
    "proj_type" = c("NNF", "NNF", rep("R&M", 4)),
    "target_species" = c("SM", "WE", "HB", "HB", "HB", "CS")
)

# write data to db
#dbWriteTable(conn = con, name = "project", value = project, append = TRUE)

# Collect data from db
proj <- tbl(con, "project") %>% 
    collect()

#-------------------------------------------
# Pi-project table
#-------------------------------------------

dbListFields(con, "pi_proj")

# Pi-project linking table
pi_proj <- tibble(
    "pi_proj_id" = c(1:6),
    "principal_id" = c(3, 4, 3, 5, 2, 2),
    "project_id" = c(1, 2, 3, 4, 5, 6)
)

# write data to db
#dbWriteTable(conn = con, name = "pi_proj", value = pi_proj, append = TRUE)

# Collect data from db
pi_pr <- tbl(con, "pi_proj") %>% 
    collect()

#------------------------------------------
# pi_proj translation table
#------------------------------------------

t_tbl <- pi_proj %>%
    left_join(proj) %>% 
    left_join(pi) %>%
    mutate(pi_name = paste(first_name, last_name, sep = " ")) %>% 
    select(pi_proj_id, project_abbr, pi_name)
#------------------------------------------
# Site-effort table (JSON col)
#------------------------------------------

dbListFields(con, "site_effort")

# Electrofishing data

el <- read_rds("./data/pass1.Rds")

el_site <- el$site %>% 
    rename_all(tolower) %>%
    mutate_at("date", mdy) %>% 
    mutate(site_id = paste(project, year(date), site_id, sep = "_"),
           data_type = "EL",
           project_abbr = tolower(project),
           startdatetime = as.POSIXct(paste(date, starttime)),
           enddatetime = as.POSIXct(paste(date, endtime)),
           el_sec = effort_sec + (effort_min * 60),
           river = "GR") %>%
    rename(site_notes = notes_site) %>% 
    left_join(t_tbl, by = "project_abbr") %>% 
    select(-c(date, starttime, endtime, effort_min, effort_sec, project_abbr, project, pi_name)) %>% 
    nest(json_col = c(pass, start_rmi, end_rmi, shoreline, boat, startdatetime, enddatetime, el_sec))

el_db <- el_site %>% 
    mutate(json_col = map_chr(json_col, toJSON, dataframe = 'rows'))
el_db$json_col[1]
str_sub(el_db$json_col, start = 1, end = 1) <- ""                       # remove opening `[`
str_sub(el_db$json_col, start = -1, end = -1) <- ""                     # remove closing `]`


names(el_db)
dbWriteTable(conn = con, name = "site_effort", value = el_db, append = TRUE)

site <- tbl(con, "site_effort") %>% 
    collect()

# Seining site data

site <- read_rds("./data/db_test_se.Rds") %>% 
    mutate(project_abbr = "SJHg") %>% 
    left_join(t_tbl, by = "project_abbr") %>% 
    select(-c(project_abbr, pi_name))


se_nest <- site %>% 
    nest(json_col = haul_id:sub_2) 

se_db <- se_nest %>% 
    mutate(json_col = map_chr(json_col, toJSON, dataframe = "rows"))

str_sub(se_db$json_col, start = 1, end = 1) <- ""                       # remove opening `[`
str_sub(se_db$json_col, start = -1, end = -1) <- ""                     # remove closing `]`
se_db$json_col[1]

dbWriteTable(conn = con, name = "site_effort", value = se_db, append = TRUE)

site_ <- tbl(con, "site_effort") %>% 
    collect() %>% 
    mutate(json_col = paste0("[", json_col, "]"))

# str_sub(site_$json_col, start = 1, end = 1) <- "["                       # add opening `[`
# str_sub(site_$json_col, start = -1, end = -1) <- "]"                     # add closing `]`

#--------------------------------------------
# Exract data
#--------------------------------------------

el_out <- site_ %>% 
    filter(data_type == "EL") %>%
    mutate(json_col = map(json_col, fromJSON)) %>% 
    unnest(cols = json_col) 

se_out <- site_ %>% 
    filter(data_type == "SE") %>% 
    mutate(json_col = map(json_col, fromJSON)) %>% 
    unnest(cols = json_col) 

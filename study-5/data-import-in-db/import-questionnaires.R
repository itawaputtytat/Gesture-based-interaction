
# Settings ----------------------------------------------------------------

sett_import <- c()
sett_import$db_name <- "GBI_Study-5"
sett_import$db_conn_name <- dbFindConnObj(sett_import$db_name, output = F)
sett_import$dir_name <- "../../Studie-5_Pro-VIL/Daten/Fragebogen_xlsx/"
sett_import$file_names <- list.files(sett_import$dir_name)

library(readxl)



# Procedure ---------------------------------------------------------------

## Set file path
## Read data
## Replace column names
## Write data to database



# Simulator sickness ------------------------------------------------------

## Pre
sett_import$file_path <- 
  file.path(sett_import$dir_name, 
            "simsick_vor.xlsx")

dat <- read_excel(sett_import$file_path, col_names = F, na = ".")

colnames(dat) <- c("id", sprintf("ssq_%02d", 1:(ncol(dat)-1)))

dbWriteTable(get(sett_import$db_conn_name), 
             "t_q_ssq_t1", 
             dat, overwrite = T, row.names = F)

## Post
sett_import$file_path <- 
  file.path(sett_import$dir_name,
            "simsick_nach.xlsx")

dat <- read_excel(sett_import$file_path, col_names = F, na = ".")

colnames(dat) <- c("id", sprintf("ssq_%02d", 1:(ncol(dat)-1)))

dbWriteTable(get(sett_import$db_conn_name), 
             "t_q_ssq_t2", 
             dat, overwrite = T, row.names = F)
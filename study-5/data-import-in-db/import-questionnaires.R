
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
            "vergleich.xlsx")

dat <- read_excel(sett_import$file_path, col_names = F, na = ".")
#dat[, c(2:29)] <- dat[, c(2:29)] - 4

colnames(dat) <- c("subject_id", sprintf("evaluation_%02d", 1:(ncol(dat)-1)))

dbWriteTable(get(sett_import$db_conn_name), 
             "t_q_evaluation", 
             dat, overwrite = T, row.names = F)

## Post
sett_import$file_path <- 
  file.path(sett_import$dir_name,
            "security_touch.xlsx")

dat <- read_excel(sett_import$file_path, col_names = F, na = ".")
#dat[, c(2:29)] <- dat[, c(2:29)] - 4

colnames(dat) <- c("subject_id", sprintf("safety_%02d", 1:(ncol(dat)-1)))

dbWriteTable(get(sett_import$db_conn_name), 
             "t_q_safety_touch", 
             dat, overwrite = T, row.names = F)